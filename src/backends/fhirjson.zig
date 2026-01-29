//! FHIR-aware JSON adapter
//!
//! This adapter wraps std.json.Value and merges the split representation of
//! FHIR primitives (field + _field) into logical nodes. Per the FHIRPath spec:
//! "specific xml or json features are not visible to the FHIRPath language
//! (such as comments and the split representation of primitives)."
//!
//! Key behaviors:
//! - Merges "field" + "_field" into unified nodes with extension/id/value children
//! - Hides "_*" keys from iteration and direct access
//! - Hides "resourceType" from iteration (but accessible via objectGet)
//! - Reports merged primitive kind as the value's primitive kind
//! - Supports objectGet("extension"), objectGet("id"), objectGet("value") on primitives

const std = @import("std");
const node = @import("../node.zig");
const item = @import("../item.zig");
const schema_mod = @import("../schema.zig");

pub const FhirNode = union(enum) {
    /// Direct passthrough to a std.json.Value
    plain: *const std.json.Value,
    /// Merged primitive: value (from "field") + metadata (from "_field")
    merged_primitive: struct {
        value: ?*const std.json.Value, // null if extension-only
        meta: ?*const std.json.Value, // null if no _field
    },
    /// Merged array: parallel value + metadata arrays
    merged_array: struct {
        values: *const std.json.Value, // the value array
        metas: ?*const std.json.Value, // the _field array (or null)
    },
};

pub const FhirJsonAdapter = struct {
    allocator: std.mem.Allocator,
    root_value: *const std.json.Value,
    nodes: std.ArrayListUnmanaged(FhirNode),
    schema: ?*schema_mod.Schema = null,

    pub const NodeRef = u32;

    pub fn init(allocator: std.mem.Allocator, root_val: *const std.json.Value) FhirJsonAdapter {
        var self = FhirJsonAdapter{
            .allocator = allocator,
            .root_value = root_val,
            .nodes = .{},
        };
        // Register root as node 0
        self.nodes.append(allocator, .{ .plain = root_val }) catch unreachable;
        return self;
    }

    pub fn deinit(self: *FhirJsonAdapter) void {
        self.nodes.deinit(self.allocator);
    }

    fn addNode(self: *FhirJsonAdapter, n: FhirNode) NodeRef {
        self.nodes.append(self.allocator, n) catch unreachable;
        return @intCast(self.nodes.items.len - 1);
    }

    inline fn getNode(self: *FhirJsonAdapter, ref: NodeRef) FhirNode {
        return self.nodes.items[ref];
    }

    pub fn root(_: *FhirJsonAdapter) NodeRef {
        return 0;
    }

    pub fn kind(self: *FhirJsonAdapter, ref: NodeRef) node.Kind {
        const n = self.getNode(ref);
        return switch (n) {
            .plain => |v| stdJsonKind(v),
            .merged_primitive => |mp| {
                if (mp.value) |v| {
                    return stdJsonKind(v);
                }
                return .null;
            },
            .merged_array => .array,
        };
    }

    pub fn objectGet(self: *FhirJsonAdapter, ref: NodeRef, key: []const u8) ?NodeRef {
        const n = self.getNode(ref);
        switch (n) {
            .plain => |v| {
                if (v.* != .object) return null;
                return self.objectGetFromObject(v, key);
            },
            .merged_primitive => |mp| {
                return self.objectGetFromMergedPrimitive(mp, key);
            },
            .merged_array => return null,
        }
    }

    fn objectGetFromObject(self: *FhirJsonAdapter, v: *const std.json.Value, key: []const u8) ?NodeRef {
        // Block direct access to underscore-prefixed keys
        if (key.len > 0 and key[0] == '_') return null;

        // Look up the key in the JSON object
        const val_ptr = v.object.getPtr(key);

        // Look up the corresponding _key
        var underscore_buf: [256]u8 = undefined;
        const meta_ptr = self.getMetaPtr(v, key, &underscore_buf);

        if (val_ptr) |vp| {
            // Value exists
            const vk = stdJsonKind(vp);
            switch (vk) {
                .string, .number, .bool, .null => {
                    // Wrap as merged primitive
                    return self.addNode(.{ .merged_primitive = .{
                        .value = vp,
                        .meta = meta_ptr,
                    } });
                },
                .array => {
                    if (meta_ptr) |mp| {
                        return self.addNode(.{ .merged_array = .{
                            .values = vp,
                            .metas = mp,
                        } });
                    }
                    // No meta - return plain array
                    return self.addNode(.{ .plain = vp });
                },
                .object => {
                    // Complex types don't merge
                    return self.addNode(.{ .plain = vp });
                },
            }
        } else if (meta_ptr) |mp| {
            // Extension-only: _key exists but key does not
            return self.addNode(.{ .merged_primitive = .{
                .value = null,
                .meta = mp,
            } });
        }

        return null;
    }

    fn objectGetFromMergedPrimitive(
        self: *FhirJsonAdapter,
        mp: anytype,
        key: []const u8,
    ) ?NodeRef {
        if (std.mem.eql(u8, key, "value")) {
            if (mp.value) |v| {
                return self.addNode(.{ .plain = v });
            }
            return null;
        }
        if (std.mem.eql(u8, key, "id")) {
            if (mp.meta) |m| {
                if (m.* == .object) {
                    if (m.object.getPtr("id")) |id_ptr| {
                        return self.addNode(.{ .plain = id_ptr });
                    }
                }
            }
            return null;
        }
        if (std.mem.eql(u8, key, "extension")) {
            if (mp.meta) |m| {
                if (m.* == .object) {
                    if (m.object.getPtr("extension")) |ext_ptr| {
                        return self.addNode(.{ .plain = ext_ptr });
                    }
                }
            }
            return null;
        }
        return null;
    }

    fn getMetaPtr(
        _: *FhirJsonAdapter,
        v: *const std.json.Value,
        key: []const u8,
        buf: *[256]u8,
    ) ?*const std.json.Value {
        if (key.len + 1 > 256) return null;
        buf[0] = '_';
        @memcpy(buf[1 .. key.len + 1], key);
        const underscore_key = buf[0 .. key.len + 1];
        return v.object.getPtr(underscore_key);
    }

    pub fn objectCount(self: *FhirJsonAdapter, ref: NodeRef) usize {
        const n = self.getNode(ref);
        switch (n) {
            .plain => |v| {
                if (v.* != .object) return 0;
                return self.countObjectFields(v);
            },
            .merged_primitive => |mp| {
                var count: usize = 0;
                if (mp.value != null) count += 1;
                if (mp.meta) |m| {
                    if (m.* == .object) {
                        if (m.object.get("id") != null) count += 1;
                        if (m.object.get("extension") != null) count += 1;
                    }
                }
                return count;
            },
            .merged_array => return 0,
        }
    }

    fn countObjectFields(_: *FhirJsonAdapter, v: *const std.json.Value) usize {
        var count: usize = 0;
        var it = v.object.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            // Skip underscore-prefixed and resourceType
            if (key.len > 0 and key[0] == '_') continue;
            if (std.mem.eql(u8, key, "resourceType")) continue;
            count += 1;
        }
        // Count extension-only fields (where _key exists but key does not)
        var it2 = v.object.iterator();
        while (it2.next()) |entry| {
            const key = entry.key_ptr.*;
            if (key.len > 1 and key[0] == '_') {
                const base_key = key[1..];
                if (v.object.get(base_key) == null) {
                    count += 1;
                }
            }
        }
        return count;
    }

    pub const ObjectIter = struct {
        entries: []const node.ObjectEntry(NodeRef),
        idx: usize,

        pub fn next(self: *ObjectIter) ?node.ObjectEntry(NodeRef) {
            if (self.idx >= self.entries.len) return null;
            const entry = self.entries[self.idx];
            self.idx += 1;
            return entry;
        }
    };

    pub fn objectIter(self: *FhirJsonAdapter, ref: NodeRef) ObjectIter {
        const n = self.getNode(ref);
        switch (n) {
            .plain => |v| {
                if (v.* != .object) return .{ .entries = &.{}, .idx = 0 };
                return self.buildObjectIter(v);
            },
            .merged_primitive => |mp| {
                return self.buildMergedPrimitiveIter(mp);
            },
            .merged_array => return .{ .entries = &.{}, .idx = 0 },
        }
    }

    fn buildObjectIter(self: *FhirJsonAdapter, v: *const std.json.Value) ObjectIter {
        // Count total entries first
        const count = self.countObjectFields(v);
        if (count == 0) return .{ .entries = &.{}, .idx = 0 };

        const entries = self.allocator.alloc(node.ObjectEntry(NodeRef), count) catch
            return .{ .entries = &.{}, .idx = 0 };

        var idx: usize = 0;

        // Phase 1: non-underscore, non-resourceType keys
        var it = v.object.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            if (key.len > 0 and key[0] == '_') continue;
            if (std.mem.eql(u8, key, "resourceType")) continue;

            // Apply merge logic via objectGet
            const child_ref = self.objectGetFromObject(v, key) orelse continue;
            entries[idx] = .{ .key = key, .value = child_ref };
            idx += 1;
        }

        // Phase 2: extension-only fields
        var it2 = v.object.iterator();
        while (it2.next()) |entry| {
            const key = entry.key_ptr.*;
            if (key.len > 1 and key[0] == '_') {
                const base_key = key[1..];
                if (v.object.get(base_key) == null) {
                    // Extension-only: create merged_primitive with null value
                    const child_ref = self.addNode(.{ .merged_primitive = .{
                        .value = null,
                        .meta = entry.value_ptr,
                    } });
                    entries[idx] = .{ .key = base_key, .value = child_ref };
                    idx += 1;
                }
            }
        }

        return .{ .entries = entries[0..idx], .idx = 0 };
    }

    fn buildMergedPrimitiveIter(self: *FhirJsonAdapter, mp: anytype) ObjectIter {
        var count: usize = 0;
        if (mp.value != null) count += 1;
        var has_id = false;
        var has_ext = false;
        if (mp.meta) |m| {
            if (m.* == .object) {
                has_id = m.object.get("id") != null;
                has_ext = m.object.get("extension") != null;
                if (has_id) count += 1;
                if (has_ext) count += 1;
            }
        }

        if (count == 0) return .{ .entries = &.{}, .idx = 0 };

        const entries = self.allocator.alloc(node.ObjectEntry(NodeRef), count) catch
            return .{ .entries = &.{}, .idx = 0 };

        var idx: usize = 0;
        if (mp.value) |v| {
            entries[idx] = .{ .key = "value", .value = self.addNode(.{ .plain = v }) };
            idx += 1;
        }
        if (has_id) {
            const id_ptr = mp.meta.?.object.getPtr("id").?;
            entries[idx] = .{ .key = "id", .value = self.addNode(.{ .plain = id_ptr }) };
            idx += 1;
        }
        if (has_ext) {
            const ext_ptr = mp.meta.?.object.getPtr("extension").?;
            entries[idx] = .{ .key = "extension", .value = self.addNode(.{ .plain = ext_ptr }) };
            idx += 1;
        }

        return .{ .entries = entries[0..idx], .idx = 0 };
    }

    pub fn arrayLen(self: *FhirJsonAdapter, ref: NodeRef) usize {
        const n = self.getNode(ref);
        return switch (n) {
            .plain => |v| if (v.* == .array) v.array.items.len else 0,
            .merged_array => |ma| ma.values.array.items.len,
            .merged_primitive => 0,
        };
    }

    pub fn arrayAt(self: *FhirJsonAdapter, ref: NodeRef, idx: usize) NodeRef {
        const n = self.getNode(ref);
        switch (n) {
            .plain => |v| {
                return self.addNode(.{ .plain = &v.array.items[idx] });
            },
            .merged_array => |ma| {
                const val_ptr = &ma.values.array.items[idx];
                const meta_ptr = self.getMergedArrayMeta(ma, idx);
                const vk = stdJsonKind(val_ptr);

                switch (vk) {
                    .string, .number, .bool, .null => {
                        return self.addNode(.{ .merged_primitive = .{
                            .value = if (val_ptr.* == .null) null else val_ptr,
                            .meta = meta_ptr,
                        } });
                    },
                    .object => {
                        // Complex elements in arrays - pass through as plain
                        return self.addNode(.{ .plain = val_ptr });
                    },
                    .array => {
                        return self.addNode(.{ .plain = val_ptr });
                    },
                }
            },
            .merged_primitive => unreachable,
        }
    }

    fn getMergedArrayMeta(
        _: *FhirJsonAdapter,
        ma: anytype,
        idx: usize,
    ) ?*const std.json.Value {
        if (ma.metas) |metas| {
            if (metas.* == .array and idx < metas.array.items.len) {
                const meta_val = &metas.array.items[idx];
                if (meta_val.* != .null) return meta_val;
            }
        }
        return null;
    }

    pub fn string(self: *FhirJsonAdapter, ref: NodeRef) []const u8 {
        const n = self.getNode(ref);
        return switch (n) {
            .plain => |v| if (v.* == .string) v.string else "",
            .merged_primitive => |mp| {
                if (mp.value) |v| {
                    if (v.* == .string) return v.string;
                }
                return "";
            },
            .merged_array => "",
        };
    }

    pub fn numberText(self: *FhirJsonAdapter, ref: NodeRef) []const u8 {
        const n = self.getNode(ref);
        const v = switch (n) {
            .plain => |p| p,
            .merged_primitive => |mp| mp.value orelse return "0",
            .merged_array => return "0",
        };
        return switch (v.*) {
            .number_string => |s| s,
            .integer => |i| std.fmt.allocPrint(self.allocator, "{d}", .{i}) catch "0",
            .float => |f| std.fmt.allocPrint(self.allocator, "{d}", .{f}) catch "0",
            else => "0",
        };
    }

    pub fn boolean(self: *FhirJsonAdapter, ref: NodeRef) bool {
        const n = self.getNode(ref);
        return switch (n) {
            .plain => |v| if (v.* == .bool) v.bool else false,
            .merged_primitive => |mp| {
                if (mp.value) |v| {
                    if (v.* == .bool) return v.bool;
                }
                return false;
            },
            .merged_array => false,
        };
    }

    // No span() or stringify() support

    /// Convert a node to a typed Value, using the schema to determine the
    /// correct System type for FHIR model types.
    pub fn toValue(self: *FhirJsonAdapter, ref: NodeRef, type_id: u32) item.Value {
        // If we have a schema and the type_id is a model type, check for implicit System type
        if (self.schema) |s| {
            if (schema_mod.isModelType(type_id)) {
                const sys_id = s.implicitSystemTypeId(type_id);
                if (sys_id != 0) {
                    const sys_name = schema_mod.systemTypeName(sys_id);
                    return self.toValueForSystemType(ref, sys_name);
                }
            }
        }
        // Fall back to JSON-kind-based conversion
        return self.toValueFromJsonKind(ref);
    }

    /// Convert using a known System type name (e.g., "System.Date", "System.Quantity")
    fn toValueForSystemType(self: *FhirJsonAdapter, ref: NodeRef, sys_name: []const u8) item.Value {
        if (std.mem.eql(u8, sys_name, "System.Boolean")) {
            return self.toValueFromJsonKind(ref);
        }
        if (std.mem.eql(u8, sys_name, "System.Integer")) {
            return self.toValueFromJsonKind(ref);
        }
        if (std.mem.eql(u8, sys_name, "System.Decimal")) {
            return self.toValueFromJsonKind(ref);
        }
        if (std.mem.eql(u8, sys_name, "System.String")) {
            return self.toValueFromJsonKind(ref);
        }
        if (std.mem.eql(u8, sys_name, "System.Date")) {
            const k = kind(self, ref);
            if (k == .string) return .{ .date = string(self, ref) };
            return self.toValueFromJsonKind(ref);
        }
        if (std.mem.eql(u8, sys_name, "System.DateTime")) {
            const k = kind(self, ref);
            if (k == .string) return .{ .dateTime = string(self, ref) };
            return self.toValueFromJsonKind(ref);
        }
        if (std.mem.eql(u8, sys_name, "System.Time")) {
            const k = kind(self, ref);
            if (k == .string) return .{ .time = string(self, ref) };
            return self.toValueFromJsonKind(ref);
        }
        if (std.mem.eql(u8, sys_name, "System.Quantity")) {
            return self.extractQuantity(ref);
        }
        // Unknown system type, fall back
        return self.toValueFromJsonKind(ref);
    }

    /// Extract a FHIR Quantity from an object node with value/code/unit fields.
    fn extractQuantity(self: *FhirJsonAdapter, ref: NodeRef) item.Value {
        const k = kind(self, ref);
        if (k != .object) return self.toValueFromJsonKind(ref);

        // Get the numeric value
        const value_ref = objectGet(self, ref, "value") orelse return .{ .empty = {} };
        const value_kind = kind(self, value_ref);
        if (value_kind != .number) return .{ .empty = {} };
        const value_text = numberText(self, value_ref);

        // Prefer "code" over "unit" (UCUM code is more precise)
        var unit_text: []const u8 = "1";
        if (objectGet(self, ref, "code")) |code_ref| {
            if (kind(self, code_ref) == .string) {
                unit_text = string(self, code_ref);
            }
        } else if (objectGet(self, ref, "unit")) |unit_ref| {
            if (kind(self, unit_ref) == .string) {
                unit_text = string(self, unit_ref);
            }
        }

        return .{ .quantity = .{ .value = value_text, .unit = unit_text } };
    }

    /// Pure JSON-kind-based conversion (no schema awareness).
    fn toValueFromJsonKind(self: *FhirJsonAdapter, ref: NodeRef) item.Value {
        return switch (kind(self, ref)) {
            .null => .{ .empty = {} },
            .bool => .{ .boolean = boolean(self, ref) },
            .number => {
                const text = numberText(self, ref);
                if (isIntegerText(text)) {
                    const parsed = std.fmt.parseInt(i64, text, 10) catch return .{ .decimal = text };
                    return .{ .integer = parsed };
                }
                return .{ .decimal = text };
            },
            .string => .{ .string = string(self, ref) },
            else => .{ .empty = {} },
        };
    }

    /// Check if number text represents an integer (no decimal point, no exponent)
    fn isIntegerText(text: []const u8) bool {
        return std.mem.indexOfScalar(u8, text, '.') == null and
            std.mem.indexOfAny(u8, text, "eE") == null;
    }

    // -- Helpers --

    fn stdJsonKind(v: *const std.json.Value) node.Kind {
        return switch (v.*) {
            .object => .object,
            .array => .array,
            .string => .string,
            .integer, .float, .number_string => .number,
            .bool => .bool,
            .null => .null,
        };
    }

    /// Convert a node back to std.json.Value for output serialization
    pub fn nodeToJsonValue(
        self: *FhirJsonAdapter,
        allocator: std.mem.Allocator,
        ref: NodeRef,
    ) !std.json.Value {
        const n = self.getNode(ref);
        switch (n) {
            .plain => |v| return cloneJsonValue(allocator, v),
            .merged_primitive => |mp| {
                // Return the primitive value (not the metadata)
                if (mp.value) |v| return cloneJsonValue(allocator, v);
                return .{ .null = {} };
            },
            .merged_array => |ma| {
                // Return the values array
                return cloneJsonValue(allocator, ma.values);
            },
        }
    }
};

fn cloneJsonValue(allocator: std.mem.Allocator, v: *const std.json.Value) !std.json.Value {
    return switch (v.*) {
        .null => .{ .null = {} },
        .bool => |b| .{ .bool = b },
        .integer => |i| .{ .integer = i },
        .float => |f| .{ .float = f },
        .number_string => |s| .{ .number_string = s },
        .string => |s| .{ .string = s },
        .array => |arr| blk: {
            var items = std.ArrayListUnmanaged(std.json.Value){};
            for (arr.items) |*item_val| {
                const cloned = try cloneJsonValue(allocator, item_val);
                try items.append(allocator, cloned);
            }
            const owned = try items.toOwnedSlice(allocator);
            break :blk .{ .array = .{ .items = owned, .capacity = owned.len, .allocator = allocator } };
        },
        .object => |obj| blk: {
            var new_obj = std.json.ObjectMap.init(allocator);
            var it = obj.iterator();
            while (it.next()) |entry| {
                const cloned = try cloneJsonValue(allocator, entry.value_ptr);
                try new_obj.put(entry.key_ptr.*, cloned);
            }
            break :blk .{ .object = new_obj };
        },
    };
}

// Verify FhirJsonAdapter conforms to NodeAdapter interface
comptime {
    node.requireAdapter(FhirJsonAdapter);
}
