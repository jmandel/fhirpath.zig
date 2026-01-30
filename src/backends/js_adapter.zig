//! JS Adapter — navigates a V8-parsed JSON object tree via WASM import callbacks.
//!
//! Instead of copying JSON text into WASM and parsing with std.json, this adapter
//! keeps the parsed JSON on the JS side and calls back into JS for every navigation
//! operation (objectGet, arrayAt, string, etc.).
//!
//! Supports both generic JSON and FHIR-aware traversal via a Flavor flag, matching
//! the JsonAdapter's behavior for field/_field merging, underscore hiding, etc.
//!
//! Node handle encoding uses two ranges within the index space:
//! - index < VIRTUAL_OFFSET (1<<24): JS-side node ID
//! - index >= VIRTUAL_OFFSET: Zig virtual node (merged primitives, quantities, etc.)

const std = @import("std");
const node = @import("../node.zig");
const nh = @import("../node_handle.zig");
const item = @import("../item.zig");
const convert = @import("../convert.zig");

/// Boundary between JS node IDs and Zig virtual node indices.
const VIRTUAL_OFFSET: usize = 1 << 24;

/// Size of the scratch buffer for string transfers across the WASM boundary.
const SCRATCH_SIZE: usize = 64 * 1024;

// ============================================================================
// WASM imports — provided by JS host
// ============================================================================

extern "env" fn js_node_kind(ref: u32) u32;
extern "env" fn js_object_get(ref: u32, key_ptr: u32, key_len: u32) u32;
extern "env" fn js_object_count(ref: u32) u32;
extern "env" fn js_object_keys(ref: u32, out_ptr: u32, max_pairs: u32) u32;
extern "env" fn js_array_len(ref: u32) u32;
extern "env" fn js_array_at(ref: u32, idx: u32) u32;
extern "env" fn js_string(ref: u32, out_ptr: u32, max_len: u32) u32;
extern "env" fn js_number_text(ref: u32, out_ptr: u32, max_len: u32) u32;
extern "env" fn js_boolean(ref: u32) u32;

// ============================================================================
// Virtual node types
// ============================================================================

const VirtualNode = union(enum) {
    /// Merged FHIR primitive: value (from "field") + metadata (from "_field")
    merged_primitive: struct {
        value: ?nh.NodeHandle, // null if extension-only
        meta: ?nh.NodeHandle, // null if no _field
    },
    /// Merged FHIR array: parallel value + metadata arrays
    merged_array: struct {
        values: nh.NodeHandle, // the value array (JS ref)
        metas: ?nh.NodeHandle, // the _field array (JS ref, or null)
    },

    // Synthetic output nodes — represent computed values as nodes
    value_scalar: item.Value,
    value_quantity: item.Quantity,
    value_typeinfo: item.TypeInfo,
};

// ============================================================================
// JS kind constants (matching JS-side enum)
// ============================================================================

const js_kind_object = 0;
const js_kind_array = 1;
const js_kind_string = 2;
const js_kind_number = 3;
const js_kind_boolean = 4;
const js_kind_null = 5;

fn jsKindToNodeKind(k: u32) node.Kind {
    return switch (k) {
        js_kind_object => .object,
        js_kind_array => .array,
        js_kind_string => .string,
        js_kind_number => .number,
        js_kind_boolean => .bool,
        else => .null,
    };
}

// ============================================================================
// JsAdapter
// ============================================================================

pub const JsAdapter = struct {
    pub const Flavor = enum {
        generic_json,
        fhir_json,
    };

    allocator: std.mem.Allocator,
    flavor: Flavor,
    root_js_id: u32,
    virtual_nodes: std.ArrayListUnmanaged(VirtualNode),
    scratch_buf: []u8,

    pub const NodeRef = nh.NodeHandle;

    pub fn init(allocator: std.mem.Allocator, root_js_id: u32, flavor: Flavor) JsAdapter {
        const scratch = allocator.alloc(u8, SCRATCH_SIZE) catch @panic("JsAdapter: scratch alloc failed");
        return .{
            .allocator = allocator,
            .flavor = flavor,
            .root_js_id = root_js_id,
            .virtual_nodes = .{},
            .scratch_buf = scratch,
        };
    }

    pub fn deinit(self: *JsAdapter) void {
        self.virtual_nodes.deinit(self.allocator);
        self.allocator.free(self.scratch_buf);
    }

    pub fn root(self: *JsAdapter) NodeRef {
        return jsIdToRef(self.root_js_id);
    }

    // ====================================================================
    // Ref encoding helpers
    // ====================================================================

    fn jsIdToRef(id: u32) NodeRef {
        return nh.fromIndex(@as(usize, id));
    }

    fn refToJsId(ref: NodeRef) u32 {
        return @intCast(nh.toIndex(ref));
    }

    fn isVirtualRef(ref: NodeRef) bool {
        if (!nh.isIndex(ref)) return false;
        return nh.toIndex(ref) >= VIRTUAL_OFFSET;
    }

    fn isJsRef(ref: NodeRef) bool {
        if (!nh.isIndex(ref)) return false;
        return nh.toIndex(ref) < VIRTUAL_OFFSET;
    }

    fn virtualIndex(ref: NodeRef) usize {
        return nh.toIndex(ref) - VIRTUAL_OFFSET;
    }

    fn addVirtualNode(self: *JsAdapter, vn: VirtualNode) NodeRef {
        self.virtual_nodes.append(self.allocator, vn) catch unreachable;
        return nh.fromIndex(VIRTUAL_OFFSET + self.virtual_nodes.items.len - 1);
    }

    fn virtualNode(self: *JsAdapter, ref: NodeRef) VirtualNode {
        return self.virtual_nodes.items[virtualIndex(ref)];
    }

    /// Read a JS string node into scratch_buf and return a slice.
    /// The returned slice points into scratch_buf and is only valid until
    /// the next readJsString call. Caller must dupe if needed.
    fn readJsString(self: *JsAdapter, js_id: u32) []const u8 {
        const len = js_string(js_id, @intCast(@intFromPtr(self.scratch_buf.ptr)), @intCast(self.scratch_buf.len));
        return self.scratch_buf[0..len];
    }

    /// Read a JS string node and dupe into arena.
    fn readJsStringDupe(self: *JsAdapter, js_id: u32) []const u8 {
        const s = self.readJsString(js_id);
        if (s.len == 0) return "";
        return self.allocator.dupe(u8, s) catch "";
    }

    // ====================================================================
    // Synthetic output nodes
    // ====================================================================

    pub fn nodeFromValue(self: *JsAdapter, val: item.Value) NodeRef {
        return switch (val) {
            .quantity => |q| self.addVirtualNode(.{ .value_quantity = q }),
            .typeInfo => |ti| self.addVirtualNode(.{ .value_typeinfo = ti }),
            else => self.addVirtualNode(.{ .value_scalar = val }),
        };
    }

    pub fn itemNode(self: *JsAdapter, it: item.Item) NodeRef {
        if (it.data_kind == .node_ref) {
            if (it.node) |n| return n;
        }
        if (it.data_kind == .value) {
            if (it.value) |v| return self.nodeFromValue(v);
        }
        return self.addVirtualNode(.{ .value_scalar = .{ .empty = {} } });
    }

    // ====================================================================
    // kind
    // ====================================================================

    pub fn kind(self: *JsAdapter, ref: NodeRef) node.Kind {
        if (isJsRef(ref)) {
            return jsKindToNodeKind(js_node_kind(refToJsId(ref)));
        }
        if (isVirtualRef(ref)) {
            const vn = self.virtualNode(ref);
            return switch (vn) {
                .merged_primitive => |mp| {
                    if (mp.value) |v| {
                        if (isJsRef(v)) return jsKindToNodeKind(js_node_kind(refToJsId(v)));
                    }
                    return .null;
                },
                .merged_array => .array,
                .value_scalar => |val| switch (val) {
                    .empty => .null,
                    .boolean => .bool,
                    .integer, .long, .decimal => .number,
                    .string, .date, .time, .dateTime => .string,
                    .quantity => .object,
                    .typeInfo => .object,
                },
                .value_quantity => .object,
                .value_typeinfo => .object,
            };
        }
        return .null;
    }

    // ====================================================================
    // objectGet
    // ====================================================================

    pub fn objectGet(self: *JsAdapter, ref: NodeRef, key: []const u8) ?NodeRef {
        if (isJsRef(ref)) {
            return switch (self.flavor) {
                .generic_json => self.genericObjectGet(ref, key),
                .fhir_json => self.fhirObjectGet(ref, key),
            };
        }
        if (isVirtualRef(ref)) {
            const vn = self.virtualNode(ref);
            return switch (vn) {
                .merged_primitive => |mp| self.mergedPrimitiveGet(mp, key),
                .merged_array => null,
                .value_quantity => |q| {
                    if (std.mem.eql(u8, key, "value")) {
                        return self.addVirtualNode(.{ .value_scalar = .{ .decimal = q.value } });
                    }
                    if (std.mem.eql(u8, key, "unit") or std.mem.eql(u8, key, "code")) {
                        return self.addVirtualNode(.{ .value_scalar = .{ .string = q.unit } });
                    }
                    return null;
                },
                .value_typeinfo => |ti| {
                    if (std.mem.eql(u8, key, "namespace")) {
                        return self.addVirtualNode(.{ .value_scalar = .{ .string = ti.namespace } });
                    }
                    if (std.mem.eql(u8, key, "name")) {
                        return self.addVirtualNode(.{ .value_scalar = .{ .string = ti.name } });
                    }
                    return null;
                },
                .value_scalar => null,
            };
        }
        return null;
    }

    fn genericObjectGet(_: *JsAdapter, ref: NodeRef, key: []const u8) ?NodeRef {
        const child_id = js_object_get(refToJsId(ref), @intCast(@intFromPtr(key.ptr)), @intCast(key.len));
        if (child_id == 0) return null;
        return jsIdToRef(child_id);
    }

    fn fhirObjectGet(self: *JsAdapter, ref: NodeRef, key: []const u8) ?NodeRef {
        // Block direct access to underscore-prefixed keys
        if (key.len > 0 and key[0] == '_') return null;

        const js_id = refToJsId(ref);
        const val_id = js_object_get(js_id, @intCast(@intFromPtr(key.ptr)), @intCast(key.len));
        const meta_id = self.getMetaId(js_id, key);

        if (val_id != 0) {
            const vk = jsKindToNodeKind(js_node_kind(val_id));
            switch (vk) {
                .string, .number, .bool, .null => {
                    return self.addVirtualNode(.{ .merged_primitive = .{
                        .value = jsIdToRef(val_id),
                        .meta = if (meta_id != 0) jsIdToRef(meta_id) else null,
                    } });
                },
                .array => {
                    return self.addVirtualNode(.{ .merged_array = .{
                        .values = jsIdToRef(val_id),
                        .metas = if (meta_id != 0) jsIdToRef(meta_id) else null,
                    } });
                },
                .object => {
                    return jsIdToRef(val_id);
                },
            }
        } else if (meta_id != 0) {
            // Extension-only: _key exists but key does not
            return self.addVirtualNode(.{ .merged_primitive = .{
                .value = null,
                .meta = jsIdToRef(meta_id),
            } });
        }

        return null;
    }

    fn mergedPrimitiveGet(self: *JsAdapter, mp: anytype, key: []const u8) ?NodeRef {
        if (std.mem.eql(u8, key, "value")) {
            return mp.value;
        }
        if (std.mem.eql(u8, key, "id")) {
            if (mp.meta) |meta_ref| {
                if (isJsRef(meta_ref)) {
                    const meta_js_id = refToJsId(meta_ref);
                    if (js_node_kind(meta_js_id) == js_kind_object) {
                        const id_id = js_object_get(meta_js_id, @intCast(@intFromPtr("id".ptr)), 2);
                        if (id_id != 0) return jsIdToRef(id_id);
                    }
                }
            }
            return null;
        }
        if (std.mem.eql(u8, key, "extension")) {
            if (mp.meta) |meta_ref| {
                if (isJsRef(meta_ref)) {
                    const meta_js_id = refToJsId(meta_ref);
                    if (js_node_kind(meta_js_id) == js_kind_object) {
                        const ext_id = js_object_get(meta_js_id, @intCast(@intFromPtr("extension".ptr)), 9);
                        if (ext_id != 0) return jsIdToRef(ext_id);
                    }
                }
            }
            return null;
        }
        _ = self;
        return null;
    }

    /// Look up the "_key" companion in the JS object. Returns JS node ID or 0.
    fn getMetaId(self: *JsAdapter, parent_js_id: u32, key: []const u8) u32 {
        if (key.len + 1 > 256) return 0;
        // Build "_key" in scratch_buf (safe: we only use a tiny prefix)
        self.scratch_buf[0] = '_';
        @memcpy(self.scratch_buf[1 .. key.len + 1], key);
        const underscore_key = self.scratch_buf[0 .. key.len + 1];
        return js_object_get(parent_js_id, @intCast(@intFromPtr(underscore_key.ptr)), @intCast(underscore_key.len));
    }

    // ====================================================================
    // objectCount
    // ====================================================================

    pub fn objectCount(self: *JsAdapter, ref: NodeRef) usize {
        if (isJsRef(ref)) {
            return switch (self.flavor) {
                .generic_json => @intCast(js_object_count(refToJsId(ref))),
                .fhir_json => self.fhirObjectCount(ref),
            };
        }
        if (isVirtualRef(ref)) {
            const vn = self.virtualNode(ref);
            return switch (vn) {
                .merged_primitive => |mp| self.mergedPrimitiveCount(mp),
                .merged_array => 0,
                .value_quantity => 2,
                .value_typeinfo => 2,
                .value_scalar => 0,
            };
        }
        return 0;
    }

    fn fhirObjectCount(self: *JsAdapter, ref: NodeRef) usize {
        const js_id = refToJsId(ref);
        const raw_count: usize = @intCast(js_object_count(js_id));
        if (raw_count == 0) return 0;

        // We need to iterate all keys to count non-underscore, non-resourceType keys
        // plus extension-only _fields.
        const pair_buf = self.allocator.alloc(u32, raw_count * 2) catch return 0;
        defer self.allocator.free(pair_buf);

        const actual = js_object_keys(js_id, @intCast(@intFromPtr(pair_buf.ptr)), @intCast(raw_count));
        const pair_count: usize = @intCast(actual);

        var count: usize = 0;

        // Phase 1: count non-underscore, non-resourceType keys
        for (0..pair_count) |i| {
            const key_id = pair_buf[i * 2];
            const key_str = self.readJsString(key_id);
            if (key_str.len > 0 and key_str[0] == '_') continue;
            if (std.mem.eql(u8, key_str, "resourceType")) continue;
            count += 1;
        }

        // Phase 2: count extension-only _fields (where base key doesn't exist)
        for (0..pair_count) |i| {
            const key_id = pair_buf[i * 2];
            const key_str = self.readJsString(key_id);
            if (key_str.len > 1 and key_str[0] == '_') {
                const base_key = key_str[1..];
                // Check if the base key exists in the object
                const base_id = js_object_get(js_id, @intCast(@intFromPtr(base_key.ptr)), @intCast(base_key.len));
                if (base_id == 0) {
                    count += 1;
                }
            }
        }

        return count;
    }

    fn mergedPrimitiveCount(_: *JsAdapter, mp: anytype) usize {
        var count: usize = 0;
        if (mp.value != null) count += 1;
        if (mp.meta) |meta_ref| {
            if (isJsRef(meta_ref)) {
                const meta_js_id = refToJsId(meta_ref);
                if (js_node_kind(meta_js_id) == js_kind_object) {
                    const id_id = js_object_get(meta_js_id, @intCast(@intFromPtr("id".ptr)), 2);
                    if (id_id != 0) count += 1;
                    const ext_id = js_object_get(meta_js_id, @intCast(@intFromPtr("extension".ptr)), 9);
                    if (ext_id != 0) count += 1;
                }
            }
        }
        return count;
    }

    // ====================================================================
    // objectIter
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

    pub fn objectIter(self: *JsAdapter, ref: NodeRef) ObjectIter {
        if (isJsRef(ref)) {
            return switch (self.flavor) {
                .generic_json => self.genericObjectIter(ref),
                .fhir_json => self.fhirObjectIter(ref),
            };
        }
        if (isVirtualRef(ref)) {
            const vn = self.virtualNode(ref);
            return switch (vn) {
                .merged_primitive => |mp| self.mergedPrimitiveIter(mp),
                .merged_array => .{ .entries = &.{}, .idx = 0 },
                .value_quantity => |q| self.syntheticQuantityIter(q),
                .value_typeinfo => |ti| self.syntheticTypeInfoIter(ti),
                .value_scalar => .{ .entries = &.{}, .idx = 0 },
            };
        }
        return .{ .entries = &.{}, .idx = 0 };
    }

    fn genericObjectIter(self: *JsAdapter, ref: NodeRef) ObjectIter {
        const js_id = refToJsId(ref);
        const count: usize = @intCast(js_object_count(js_id));
        if (count == 0) return .{ .entries = &.{}, .idx = 0 };

        const pair_buf = self.allocator.alloc(u32, count * 2) catch
            return .{ .entries = &.{}, .idx = 0 };
        defer self.allocator.free(pair_buf);

        const actual = js_object_keys(js_id, @intCast(@intFromPtr(pair_buf.ptr)), @intCast(count));
        const pair_count: usize = @intCast(actual);
        if (pair_count == 0) return .{ .entries = &.{}, .idx = 0 };

        const entries = self.allocator.alloc(node.ObjectEntry(NodeRef), pair_count) catch
            return .{ .entries = &.{}, .idx = 0 };

        for (0..pair_count) |i| {
            const key_id = pair_buf[i * 2];
            const val_id = pair_buf[i * 2 + 1];
            entries[i] = .{
                .key = self.readJsStringDupe(key_id),
                .value = jsIdToRef(val_id),
            };
        }

        return .{ .entries = entries[0..pair_count], .idx = 0 };
    }

    fn fhirObjectIter(self: *JsAdapter, ref: NodeRef) ObjectIter {
        const count = self.fhirObjectCount(ref);
        if (count == 0) return .{ .entries = &.{}, .idx = 0 };

        const js_id = refToJsId(ref);
        const raw_count: usize = @intCast(js_object_count(js_id));
        const pair_buf = self.allocator.alloc(u32, raw_count * 2) catch
            return .{ .entries = &.{}, .idx = 0 };
        defer self.allocator.free(pair_buf);

        const actual = js_object_keys(js_id, @intCast(@intFromPtr(pair_buf.ptr)), @intCast(raw_count));
        const pair_count: usize = @intCast(actual);

        const entries = self.allocator.alloc(node.ObjectEntry(NodeRef), count) catch
            return .{ .entries = &.{}, .idx = 0 };

        var idx: usize = 0;

        // Phase 1: non-underscore, non-resourceType keys
        for (0..pair_count) |i| {
            const key_id = pair_buf[i * 2];
            const key_str = self.readJsStringDupe(key_id);
            if (key_str.len > 0 and key_str[0] == '_') continue;
            if (std.mem.eql(u8, key_str, "resourceType")) continue;

            const child_ref = self.fhirObjectGet(ref, key_str) orelse continue;
            entries[idx] = .{ .key = key_str, .value = child_ref };
            idx += 1;
        }

        // Phase 2: extension-only fields (_key exists, key doesn't)
        for (0..pair_count) |i| {
            const key_id = pair_buf[i * 2];
            const key_str = self.readJsStringDupe(key_id);
            if (key_str.len > 1 and key_str[0] == '_') {
                const base_key = key_str[1..];
                const base_id = js_object_get(js_id, @intCast(@intFromPtr(base_key.ptr)), @intCast(base_key.len));
                if (base_id == 0) {
                    // Extension-only: create merged_primitive with no value
                    const val_id = pair_buf[i * 2 + 1];
                    const child_ref = self.addVirtualNode(.{ .merged_primitive = .{
                        .value = null,
                        .meta = jsIdToRef(val_id),
                    } });
                    // Arena-dupe the base_key since it may reference scratch
                    const duped_base = self.allocator.dupe(u8, base_key) catch continue;
                    entries[idx] = .{ .key = duped_base, .value = child_ref };
                    idx += 1;
                }
            }
        }

        return .{ .entries = entries[0..idx], .idx = 0 };
    }

    fn mergedPrimitiveIter(self: *JsAdapter, mp: anytype) ObjectIter {
        var count: usize = 0;
        if (mp.value != null) count += 1;
        var has_id = false;
        var has_ext = false;
        var id_js_id: u32 = 0;
        var ext_js_id: u32 = 0;

        if (mp.meta) |meta_ref| {
            if (isJsRef(meta_ref)) {
                const meta_js = refToJsId(meta_ref);
                if (js_node_kind(meta_js) == js_kind_object) {
                    id_js_id = js_object_get(meta_js, @intCast(@intFromPtr("id".ptr)), 2);
                    has_id = id_js_id != 0;
                    ext_js_id = js_object_get(meta_js, @intCast(@intFromPtr("extension".ptr)), 9);
                    has_ext = ext_js_id != 0;
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
            entries[idx] = .{ .key = "id", .value = jsIdToRef(id_js_id) };
            idx += 1;
        }
        if (has_ext) {
            entries[idx] = .{ .key = "extension", .value = jsIdToRef(ext_js_id) };
            idx += 1;
        }

        return .{ .entries = entries[0..idx], .idx = 0 };
    }

    fn syntheticQuantityIter(self: *JsAdapter, q: item.Quantity) ObjectIter {
        const entries = self.allocator.alloc(node.ObjectEntry(NodeRef), 2) catch
            return .{ .entries = &.{}, .idx = 0 };
        entries[0] = .{ .key = "value", .value = self.addVirtualNode(.{ .value_scalar = .{ .decimal = q.value } }) };
        entries[1] = .{ .key = "unit", .value = self.addVirtualNode(.{ .value_scalar = .{ .string = q.unit } }) };
        return .{ .entries = entries, .idx = 0 };
    }

    fn syntheticTypeInfoIter(self: *JsAdapter, ti: item.TypeInfo) ObjectIter {
        const entries = self.allocator.alloc(node.ObjectEntry(NodeRef), 2) catch
            return .{ .entries = &.{}, .idx = 0 };
        entries[0] = .{ .key = "namespace", .value = self.addVirtualNode(.{ .value_scalar = .{ .string = ti.namespace } }) };
        entries[1] = .{ .key = "name", .value = self.addVirtualNode(.{ .value_scalar = .{ .string = ti.name } }) };
        return .{ .entries = entries, .idx = 0 };
    }

    // ====================================================================
    // arrayLen, arrayAt
    // ====================================================================

    pub fn arrayLen(self: *JsAdapter, ref: NodeRef) usize {
        if (isJsRef(ref)) {
            return @intCast(js_array_len(refToJsId(ref)));
        }
        if (isVirtualRef(ref)) {
            const vn = self.virtualNode(ref);
            return switch (vn) {
                .merged_array => |ma| @intCast(js_array_len(refToJsId(ma.values))),
                .merged_primitive, .value_scalar, .value_quantity, .value_typeinfo => 0,
            };
        }
        return 0;
    }

    pub fn arrayAt(self: *JsAdapter, ref: NodeRef, idx: usize) NodeRef {
        if (isJsRef(ref)) {
            const child_id = js_array_at(refToJsId(ref), @intCast(idx));
            return jsIdToRef(child_id);
        }
        if (isVirtualRef(ref)) {
            const vn = self.virtualNode(ref);
            switch (vn) {
                .merged_array => |ma| {
                    const val_id = js_array_at(refToJsId(ma.values), @intCast(idx));
                    const meta_ref = self.getMergedArrayMeta(ma, idx);
                    const vk = jsKindToNodeKind(js_node_kind(val_id));

                    switch (vk) {
                        .string, .number, .bool, .null => {
                            return self.addVirtualNode(.{ .merged_primitive = .{
                                .value = if (vk == .null) null else jsIdToRef(val_id),
                                .meta = meta_ref,
                            } });
                        },
                        .object, .array => {
                            return jsIdToRef(val_id);
                        },
                    }
                },
                .merged_primitive, .value_scalar, .value_quantity, .value_typeinfo => unreachable,
            }
        }
        unreachable;
    }

    fn getMergedArrayMeta(self: *JsAdapter, ma: anytype, idx: usize) ?NodeRef {
        _ = self;
        if (ma.metas) |metas_ref| {
            if (isJsRef(metas_ref)) {
                const metas_js = refToJsId(metas_ref);
                if (js_node_kind(metas_js) == js_kind_array) {
                    const len: usize = @intCast(js_array_len(metas_js));
                    if (idx < len) {
                        const meta_id = js_array_at(metas_js, @intCast(idx));
                        if (js_node_kind(meta_id) != js_kind_null) {
                            return jsIdToRef(meta_id);
                        }
                    }
                }
            }
        }
        return null;
    }

    // ====================================================================
    // Scalar accessors
    // ====================================================================

    pub fn string(self: *JsAdapter, ref: NodeRef) []const u8 {
        if (isJsRef(ref)) {
            const len = js_string(refToJsId(ref), @intCast(@intFromPtr(self.scratch_buf.ptr)), @intCast(self.scratch_buf.len));
            if (len == 0) return "";
            return self.allocator.dupe(u8, self.scratch_buf[0..len]) catch "";
        }
        if (isVirtualRef(ref)) {
            const vn = self.virtualNode(ref);
            return switch (vn) {
                .merged_primitive => |mp| {
                    if (mp.value) |v_ref| {
                        if (isJsRef(v_ref)) {
                            const len = js_string(refToJsId(v_ref), @intCast(@intFromPtr(self.scratch_buf.ptr)), @intCast(self.scratch_buf.len));
                            if (len == 0) return "";
                            return self.allocator.dupe(u8, self.scratch_buf[0..len]) catch "";
                        }
                    }
                    return "";
                },
                .merged_array => "",
                .value_scalar => |val| switch (val) {
                    .string, .date, .time, .dateTime => |s| s,
                    .decimal => |s| s,
                    else => "",
                },
                .value_quantity, .value_typeinfo => "",
            };
        }
        return "";
    }

    pub fn numberText(self: *JsAdapter, ref: NodeRef) []const u8 {
        if (isJsRef(ref)) {
            const len = js_number_text(refToJsId(ref), @intCast(@intFromPtr(self.scratch_buf.ptr)), @intCast(self.scratch_buf.len));
            if (len == 0) return "0";
            return self.allocator.dupe(u8, self.scratch_buf[0..len]) catch "0";
        }
        if (isVirtualRef(ref)) {
            const vn = self.virtualNode(ref);
            return switch (vn) {
                .merged_primitive => |mp| {
                    if (mp.value) |v_ref| {
                        if (isJsRef(v_ref)) {
                            const len = js_number_text(refToJsId(v_ref), @intCast(@intFromPtr(self.scratch_buf.ptr)), @intCast(self.scratch_buf.len));
                            if (len == 0) return "0";
                            return self.allocator.dupe(u8, self.scratch_buf[0..len]) catch "0";
                        }
                    }
                    return "0";
                },
                .merged_array => "0",
                .value_scalar => |val| switch (val) {
                    .integer => |i| std.fmt.allocPrint(self.allocator, "{d}", .{i}) catch "0",
                    .long => |l| std.fmt.allocPrint(self.allocator, "{d}", .{l}) catch "0",
                    .decimal => |s| s,
                    else => "0",
                },
                .value_quantity => |q| q.value,
                .value_typeinfo => "0",
            };
        }
        return "0";
    }

    pub fn boolean(self: *JsAdapter, ref: NodeRef) bool {
        if (isJsRef(ref)) {
            return js_boolean(refToJsId(ref)) != 0;
        }
        if (isVirtualRef(ref)) {
            const vn = self.virtualNode(ref);
            return switch (vn) {
                .merged_primitive => |mp| {
                    if (mp.value) |v_ref| {
                        if (isJsRef(v_ref)) return js_boolean(refToJsId(v_ref)) != 0;
                    }
                    return false;
                },
                .merged_array => false,
                .value_scalar => |val| switch (val) {
                    .boolean => |b| b,
                    else => false,
                },
                .value_quantity, .value_typeinfo => false,
            };
        }
        return false;
    }

    // ====================================================================
    // nodeToJsonValue — for resolveResult serialization
    // ====================================================================

    pub fn nodeToJsonValue(
        self: *JsAdapter,
        allocator: std.mem.Allocator,
        ref: NodeRef,
    ) !std.json.Value {
        return convert.adapterNodeToJsonValue(JsAdapter, allocator, self, ref);
    }
};

// Verify JsAdapter conforms to NodeAdapter interface
comptime {
    node.requireAdapter(JsAdapter);
}
