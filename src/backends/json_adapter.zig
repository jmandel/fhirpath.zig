//! Unified JSON adapter with flavor support.
//!
//! This adapter handles both generic JSON and FHIR-aware JSON traversal
//! through a single implementation with a `Flavor` enum. It uses the
//! NodeHandle convention from node_handle.zig:
//!
//! - Pointer-backed nodes (LSB=0): direct *const std.json.Value pointers
//! - Index-backed virtual nodes (LSB=1): entries in the virtual_nodes table
//!
//! Generic JSON flavor: plain object/array traversal, no hiding, no merging.
//! FHIR JSON flavor: merges field + _field primitives, hides underscore keys,
//! hides resourceType from iteration.

const std = @import("std");
const node = @import("../node.zig");
const nh = @import("../node_handle.zig");
const item = @import("../item.zig");
const schema_mod = @import("../schema.zig");

pub const VirtualNode = union(enum) {
    /// Merged FHIR primitive: value (from "field") + metadata (from "_field")
    merged_primitive: struct {
        value: ?nh.NodeHandle, // null if extension-only
        meta: ?nh.NodeHandle, // null if no _field
    },
    /// Merged FHIR array: parallel value + metadata arrays
    merged_array: struct {
        values: nh.NodeHandle, // the value array
        metas: ?nh.NodeHandle, // the _field array (or null)
    },
};

pub const JsonAdapter = struct {
    pub const Flavor = enum {
        generic_json,
        fhir_json,
    };

    allocator: std.mem.Allocator,
    flavor: Flavor,
    root_handle: nh.NodeHandle,
    virtual_nodes: std.ArrayListUnmanaged(VirtualNode),
    schema: ?*schema_mod.Schema = null,

    pub const NodeRef = nh.NodeHandle;

    pub fn init(allocator: std.mem.Allocator, root_val: *const std.json.Value, flavor: Flavor) JsonAdapter {
        return .{
            .allocator = allocator,
            .flavor = flavor,
            .root_handle = nh.fromPtr(std.json.Value, root_val),
            .virtual_nodes = .{},
        };
    }

    pub fn deinit(self: *JsonAdapter) void {
        self.virtual_nodes.deinit(self.allocator);
    }

    pub fn root(self: *JsonAdapter) NodeRef {
        return self.root_handle;
    }

    // -- Internal helpers --

    fn addVirtualNode(self: *JsonAdapter, vn: VirtualNode) NodeRef {
        self.virtual_nodes.append(self.allocator, vn) catch unreachable;
        return nh.fromIndex(self.virtual_nodes.items.len - 1);
    }

    inline fn jsonPtr(handle: NodeRef) *const std.json.Value {
        return nh.toPtr(std.json.Value, handle);
    }

    inline fn virtualNode(self: *JsonAdapter, handle: NodeRef) VirtualNode {
        return self.virtual_nodes.items[nh.toIndex(handle)];
    }

    fn ptrHandle(v: *const std.json.Value) NodeRef {
        return nh.fromPtr(std.json.Value, v);
    }

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

    // ====================================================================
    // Adapter contract: kind
    // ====================================================================

    pub fn kind(self: *JsonAdapter, ref: NodeRef) node.Kind {
        if (!nh.isIndex(ref)) {
            return stdJsonKind(jsonPtr(ref));
        }
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .merged_primitive => |mp| {
                if (mp.value) |v| {
                    if (!nh.isIndex(v)) return stdJsonKind(jsonPtr(v));
                    // Shouldn't normally happen—merged primitive values are ptr-backed
                    return .null;
                }
                return .null;
            },
            .merged_array => .array,
        };
    }

    // ====================================================================
    // Adapter contract: objectGet
    // ====================================================================

    pub fn objectGet(self: *JsonAdapter, ref: NodeRef, key: []const u8) ?NodeRef {
        if (!nh.isIndex(ref)) {
            const v = jsonPtr(ref);
            if (v.* != .object) return null;
            return switch (self.flavor) {
                .generic_json => self.genericObjectGet(v, key),
                .fhir_json => self.fhirObjectGet(v, key),
            };
        }
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .merged_primitive => |mp| self.mergedPrimitiveGet(mp, key),
            .merged_array => null,
        };
    }

    fn genericObjectGet(_: *JsonAdapter, v: *const std.json.Value, key: []const u8) ?NodeRef {
        const ptr = v.object.getPtr(key) orelse return null;
        return ptrHandle(ptr);
    }

    fn fhirObjectGet(self: *JsonAdapter, v: *const std.json.Value, key: []const u8) ?NodeRef {
        // Block direct access to underscore-prefixed keys
        if (key.len > 0 and key[0] == '_') return null;

        const val_ptr = v.object.getPtr(key);
        const meta_ptr = self.getMetaPtr(v, key);

        if (val_ptr) |vp| {
            const vk = stdJsonKind(vp);
            switch (vk) {
                .string, .number, .bool, .null => {
                    return self.addVirtualNode(.{ .merged_primitive = .{
                        .value = ptrHandle(vp),
                        .meta = if (meta_ptr) |mp| ptrHandle(mp) else null,
                    } });
                },
                .array => {
                    if (meta_ptr) |mp| {
                        return self.addVirtualNode(.{ .merged_array = .{
                            .values = ptrHandle(vp),
                            .metas = ptrHandle(mp),
                        } });
                    }
                    return ptrHandle(vp);
                },
                .object => {
                    return ptrHandle(vp);
                },
            }
        } else if (meta_ptr) |mp| {
            // Extension-only: _key exists but key does not
            return self.addVirtualNode(.{ .merged_primitive = .{
                .value = null,
                .meta = ptrHandle(mp),
            } });
        }

        return null;
    }

    fn mergedPrimitiveGet(self: *JsonAdapter, mp: anytype, key: []const u8) ?NodeRef {
        if (std.mem.eql(u8, key, "value")) {
            if (mp.value) |v| {
                // value is already a NodeHandle pointing to the raw JSON value
                // Wrap in a plain virtual node? No—just return it directly if ptr-backed.
                // But we need to be careful: this should be treated as a plain node.
                return v;
            }
            return null;
        }
        if (std.mem.eql(u8, key, "id")) {
            if (mp.meta) |meta_handle| {
                const m = if (!nh.isIndex(meta_handle)) jsonPtr(meta_handle) else return null;
                if (m.* == .object) {
                    if (m.object.getPtr("id")) |id_ptr| {
                        return ptrHandle(id_ptr);
                    }
                }
            }
            return null;
        }
        if (std.mem.eql(u8, key, "extension")) {
            if (mp.meta) |meta_handle| {
                const m = if (!nh.isIndex(meta_handle)) jsonPtr(meta_handle) else return null;
                if (m.* == .object) {
                    if (m.object.getPtr("extension")) |ext_ptr| {
                        return ptrHandle(ext_ptr);
                    }
                }
            }
            return null;
        }
        _ = self;
        return null;
    }

    fn getMetaPtr(_: *JsonAdapter, v: *const std.json.Value, key: []const u8) ?*const std.json.Value {
        if (key.len + 1 > 256) return null;
        var buf: [256]u8 = undefined;
        buf[0] = '_';
        @memcpy(buf[1 .. key.len + 1], key);
        const underscore_key = buf[0 .. key.len + 1];
        return v.object.getPtr(underscore_key);
    }

    // ====================================================================
    // Adapter contract: objectCount
    // ====================================================================

    pub fn objectCount(self: *JsonAdapter, ref: NodeRef) usize {
        if (!nh.isIndex(ref)) {
            const v = jsonPtr(ref);
            if (v.* != .object) return 0;
            return switch (self.flavor) {
                .generic_json => v.object.count(),
                .fhir_json => self.fhirObjectCount(v),
            };
        }
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .merged_primitive => |mp| self.mergedPrimitiveCount(mp),
            .merged_array => 0,
        };
    }

    fn fhirObjectCount(self: *JsonAdapter, v: *const std.json.Value) usize {
        _ = self;
        var count: usize = 0;
        var it = v.object.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            if (key.len > 0 and key[0] == '_') continue;
            if (std.mem.eql(u8, key, "resourceType")) continue;
            count += 1;
        }
        // Count extension-only fields
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

    fn mergedPrimitiveCount(_: *JsonAdapter, mp: anytype) usize {
        var count: usize = 0;
        if (mp.value != null) count += 1;
        if (mp.meta) |meta_handle| {
            if (!nh.isIndex(meta_handle)) {
                const m = jsonPtr(meta_handle);
                if (m.* == .object) {
                    if (m.object.get("id") != null) count += 1;
                    if (m.object.get("extension") != null) count += 1;
                }
            }
        }
        return count;
    }

    // ====================================================================
    // Adapter contract: objectIter
    // ====================================================================

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

    pub fn objectIter(self: *JsonAdapter, ref: NodeRef) ObjectIter {
        if (!nh.isIndex(ref)) {
            const v = jsonPtr(ref);
            if (v.* != .object) return .{ .entries = &.{}, .idx = 0 };
            return switch (self.flavor) {
                .generic_json => self.genericObjectIter(v),
                .fhir_json => self.fhirObjectIter(v),
            };
        }
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .merged_primitive => |mp| self.mergedPrimitiveIter(mp),
            .merged_array => .{ .entries = &.{}, .idx = 0 },
        };
    }

    fn genericObjectIter(self: *JsonAdapter, v: *const std.json.Value) ObjectIter {
        const count = v.object.count();
        if (count == 0) return .{ .entries = &.{}, .idx = 0 };

        const entries = self.allocator.alloc(node.ObjectEntry(NodeRef), count) catch
            return .{ .entries = &.{}, .idx = 0 };

        var idx: usize = 0;
        var it = v.object.iterator();
        while (it.next()) |entry| {
            entries[idx] = .{ .key = entry.key_ptr.*, .value = ptrHandle(entry.value_ptr) };
            idx += 1;
        }
        return .{ .entries = entries[0..idx], .idx = 0 };
    }

    fn fhirObjectIter(self: *JsonAdapter, v: *const std.json.Value) ObjectIter {
        const count = self.fhirObjectCount(v);
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

            const child_ref = self.fhirObjectGet(v, key) orelse continue;
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
                    const child_ref = self.addVirtualNode(.{ .merged_primitive = .{
                        .value = null,
                        .meta = ptrHandle(entry.value_ptr),
                    } });
                    entries[idx] = .{ .key = base_key, .value = child_ref };
                    idx += 1;
                }
            }
        }

        return .{ .entries = entries[0..idx], .idx = 0 };
    }

    fn mergedPrimitiveIter(self: *JsonAdapter, mp: anytype) ObjectIter {
        var count: usize = 0;
        if (mp.value != null) count += 1;
        var has_id = false;
        var has_ext = false;
        if (mp.meta) |meta_handle| {
            if (!nh.isIndex(meta_handle)) {
                const m = jsonPtr(meta_handle);
                if (m.* == .object) {
                    has_id = m.object.get("id") != null;
                    has_ext = m.object.get("extension") != null;
                    if (has_id) count += 1;
                    if (has_ext) count += 1;
                }
            }
        }

        if (count == 0) return .{ .entries = &.{}, .idx = 0 };

        const entries = self.allocator.alloc(node.ObjectEntry(NodeRef), count) catch
            return .{ .entries = &.{}, .idx = 0 };

        var idx: usize = 0;
        if (mp.value) |v| {
            entries[idx] = .{ .key = "value", .value = v };
            idx += 1;
        }
        if (has_id) {
            const m = jsonPtr(mp.meta.?);
            const id_ptr = m.object.getPtr("id").?;
            entries[idx] = .{ .key = "id", .value = ptrHandle(id_ptr) };
            idx += 1;
        }
        if (has_ext) {
            const m = jsonPtr(mp.meta.?);
            const ext_ptr = m.object.getPtr("extension").?;
            entries[idx] = .{ .key = "extension", .value = ptrHandle(ext_ptr) };
            idx += 1;
        }

        return .{ .entries = entries[0..idx], .idx = 0 };
    }

    // ====================================================================
    // Adapter contract: arrayLen, arrayAt
    // ====================================================================

    pub fn arrayLen(self: *JsonAdapter, ref: NodeRef) usize {
        if (!nh.isIndex(ref)) {
            const v = jsonPtr(ref);
            if (v.* != .array) return 0;
            return v.array.items.len;
        }
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .merged_array => |ma| {
                const values = jsonPtr(ma.values);
                if (values.* == .array) return values.array.items.len;
                return 0;
            },
            .merged_primitive => 0,
        };
    }

    pub fn arrayAt(self: *JsonAdapter, ref: NodeRef, idx: usize) NodeRef {
        if (!nh.isIndex(ref)) {
            const v = jsonPtr(ref);
            return ptrHandle(&v.array.items[idx]);
        }
        const vn = self.virtualNode(ref);
        switch (vn) {
            .merged_array => |ma| {
                const values = jsonPtr(ma.values);
                const val_ptr = &values.array.items[idx];
                const meta_ptr = self.getMergedArrayMeta(ma, idx);
                const vk = stdJsonKind(val_ptr);

                switch (vk) {
                    .string, .number, .bool, .null => {
                        return self.addVirtualNode(.{ .merged_primitive = .{
                            .value = if (val_ptr.* == .null) null else ptrHandle(val_ptr),
                            .meta = meta_ptr,
                        } });
                    },
                    .object, .array => {
                        return ptrHandle(val_ptr);
                    },
                }
            },
            .merged_primitive => unreachable,
        }
    }

    fn getMergedArrayMeta(self: *JsonAdapter, ma: anytype, idx: usize) ?NodeRef {
        _ = self;
        if (ma.metas) |metas_handle| {
            if (!nh.isIndex(metas_handle)) {
                const metas = jsonPtr(metas_handle);
                if (metas.* == .array and idx < metas.array.items.len) {
                    const meta_val = &metas.array.items[idx];
                    if (meta_val.* != .null) return ptrHandle(meta_val);
                }
            }
        }
        return null;
    }

    // ====================================================================
    // Adapter contract: scalar accessors
    // ====================================================================

    pub fn string(self: *JsonAdapter, ref: NodeRef) []const u8 {
        if (!nh.isIndex(ref)) {
            const v = jsonPtr(ref);
            return if (v.* == .string) v.string else "";
        }
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .merged_primitive => |mp| {
                if (mp.value) |v_handle| {
                    if (!nh.isIndex(v_handle)) {
                        const v = jsonPtr(v_handle);
                        if (v.* == .string) return v.string;
                    }
                }
                return "";
            },
            .merged_array => "",
        };
    }

    pub fn numberText(self: *JsonAdapter, ref: NodeRef) []const u8 {
        const v = self.resolveToJsonValue(ref) orelse return "0";
        return switch (v.*) {
            .number_string => |s| s,
            .integer => |i| std.fmt.allocPrint(self.allocator, "{d}", .{i}) catch "0",
            .float => |f| std.fmt.allocPrint(self.allocator, "{d}", .{f}) catch "0",
            else => "0",
        };
    }

    pub fn boolean(self: *JsonAdapter, ref: NodeRef) bool {
        if (!nh.isIndex(ref)) {
            const v = jsonPtr(ref);
            return if (v.* == .bool) v.bool else false;
        }
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .merged_primitive => |mp| {
                if (mp.value) |v_handle| {
                    if (!nh.isIndex(v_handle)) {
                        const v = jsonPtr(v_handle);
                        if (v.* == .bool) return v.bool;
                    }
                }
                return false;
            },
            .merged_array => false,
        };
    }

    /// Resolve a handle to the underlying std.json.Value pointer (for scalars).
    /// Returns null for virtual nodes that don't have a backing value.
    fn resolveToJsonValue(self: *JsonAdapter, ref: NodeRef) ?*const std.json.Value {
        if (!nh.isIndex(ref)) return jsonPtr(ref);
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .merged_primitive => |mp| {
                if (mp.value) |v_handle| {
                    if (!nh.isIndex(v_handle)) return jsonPtr(v_handle);
                }
                return null;
            },
            .merged_array => null,
        };
    }

    // ====================================================================
    // toValue — type-aware value conversion
    // ====================================================================

    pub fn toValue(self: *JsonAdapter, ref: NodeRef, type_id: u32) item.Value {
        // Schema-aware path (FHIR flavor with schema)
        if (self.schema) |s| {
            if (schema_mod.isModelType(type_id)) {
                const sys_id = s.implicitSystemTypeId(type_id);
                if (sys_id != 0) {
                    const sys_name = schema_mod.systemTypeName(sys_id);
                    return self.toValueForSystemType(ref, sys_name);
                }
            }
        }
        // Well-known System type IDs
        if (type_id == item.SystemTypeIds.date) {
            if (self.kind(ref) == .string) return .{ .date = self.string(ref) };
        } else if (type_id == item.SystemTypeIds.dateTime) {
            if (self.kind(ref) == .string) return .{ .dateTime = self.string(ref) };
        } else if (type_id == item.SystemTypeIds.time) {
            if (self.kind(ref) == .string) return .{ .time = self.string(ref) };
        }
        // JSON-kind-based fallback
        return self.toValueFromJsonKind(ref);
    }

    fn toValueForSystemType(self: *JsonAdapter, ref: NodeRef, sys_name: []const u8) item.Value {
        if (std.mem.eql(u8, sys_name, "System.Date")) {
            if (self.kind(ref) == .string) return .{ .date = self.string(ref) };
            return self.toValueFromJsonKind(ref);
        }
        if (std.mem.eql(u8, sys_name, "System.DateTime")) {
            if (self.kind(ref) == .string) return .{ .dateTime = self.string(ref) };
            return self.toValueFromJsonKind(ref);
        }
        if (std.mem.eql(u8, sys_name, "System.Time")) {
            if (self.kind(ref) == .string) return .{ .time = self.string(ref) };
            return self.toValueFromJsonKind(ref);
        }
        if (std.mem.eql(u8, sys_name, "System.Quantity")) {
            return self.extractQuantity(ref);
        }
        // Boolean, Integer, Decimal, String — use JSON-kind conversion
        return self.toValueFromJsonKind(ref);
    }

    fn extractQuantity(self: *JsonAdapter, ref: NodeRef) item.Value {
        if (self.kind(ref) != .object) return self.toValueFromJsonKind(ref);

        const value_ref = self.objectGet(ref, "value") orelse return .{ .empty = {} };
        if (self.kind(value_ref) != .number) return .{ .empty = {} };
        const value_text = self.numberText(value_ref);

        var unit_text: []const u8 = "1";
        if (self.objectGet(ref, "code")) |code_ref| {
            if (self.kind(code_ref) == .string) {
                unit_text = self.string(code_ref);
            }
        } else if (self.objectGet(ref, "unit")) |unit_ref| {
            if (self.kind(unit_ref) == .string) {
                unit_text = self.string(unit_ref);
            }
        }

        return .{ .quantity = .{ .value = value_text, .unit = unit_text } };
    }

    fn toValueFromJsonKind(self: *JsonAdapter, ref: NodeRef) item.Value {
        return switch (self.kind(ref)) {
            .null => .{ .empty = {} },
            .bool => .{ .boolean = self.boolean(ref) },
            .number => {
                const text = self.numberText(ref);
                if (isIntegerText(text)) {
                    const parsed = std.fmt.parseInt(i64, text, 10) catch return .{ .decimal = text };
                    return .{ .integer = parsed };
                }
                return .{ .decimal = text };
            },
            .string => .{ .string = self.string(ref) },
            else => .{ .empty = {} },
        };
    }

    fn isIntegerText(text: []const u8) bool {
        return std.mem.indexOfScalar(u8, text, '.') == null and
            std.mem.indexOfAny(u8, text, "eE") == null;
    }

    // ====================================================================
    // nodeToJsonValue — for resolveResult serialization
    // ====================================================================

    /// Convert a node back to std.json.Value for output serialization.
    /// This is used by resolveResult to materialize FHIR virtual nodes.
    pub fn nodeToJsonValue(
        self: *JsonAdapter,
        allocator: std.mem.Allocator,
        ref: NodeRef,
    ) !std.json.Value {
        if (!nh.isIndex(ref)) {
            return cloneJsonValue(allocator, jsonPtr(ref));
        }
        const vn = self.virtualNode(ref);
        switch (vn) {
            .merged_primitive => |mp| {
                if (mp.value) |v_handle| {
                    if (!nh.isIndex(v_handle)) return cloneJsonValue(allocator, jsonPtr(v_handle));
                }
                return .{ .null = {} };
            },
            .merged_array => |ma| {
                if (!nh.isIndex(ma.values)) {
                    return cloneJsonValue(allocator, jsonPtr(ma.values));
                }
                return .{ .null = {} };
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

// Verify JsonAdapter conforms to NodeAdapter interface
comptime {
    node.requireAdapter(JsonAdapter);
}
