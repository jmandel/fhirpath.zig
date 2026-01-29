//! FHIR XML adapter implementing the NodeAdapter contract from node.zig.
//!
//! Maps FHIR XML elements to the same abstract model that JsonAdapter uses:
//!
//! - Elements with children → `.object`
//! - Elements with `value` attribute and no children → scalar (`.string`/`.number`/`.bool`)
//! - `<div xmlns="...">` → `.string` (XHTML serialized)
//! - Repeated child elements with same tag → virtual array nodes
//! - `resourceType` → synthesized from root element tag name
//! - Contained resources → `<contained><Observation>...</Observation></contained>`
//!
//! Uses the NodeHandle convention from node_handle.zig:
//! - Pointer-backed nodes (LSB=0): direct *const XmlNode pointers
//! - Index-backed virtual nodes (LSB=1): entries in the virtual_nodes table

const std = @import("std");
const node = @import("../node.zig");
const nh = @import("../node_handle.zig");
const item = @import("../item.zig");
const xml_parser = @import("xml_parser.zig");

pub const XmlNode = xml_parser.XmlNode;

const VirtualNode = union(enum) {
    /// Array of repeated child elements with the same tag
    child_array: struct {
        parent: *const XmlNode,
        tag: []const u8,
    },
    /// Synthetic string for resourceType
    resource_type_string: []const u8,
    /// Synthetic output nodes (same as JsonAdapter)
    value_scalar: item.Value,
    value_quantity: item.Quantity,
    value_typeinfo: item.TypeInfo,
    /// Wrapper for contained resource (the inner element like <Observation>)
    contained_resource: *const XmlNode,
};

pub const XmlAdapter = struct {
    allocator: std.mem.Allocator,
    root_node: *const XmlNode,
    root_handle: nh.NodeHandle,
    virtual_nodes: std.ArrayListUnmanaged(VirtualNode),

    pub const NodeRef = nh.NodeHandle;

    pub fn init(allocator: std.mem.Allocator, root_node: *const XmlNode) XmlAdapter {
        return .{
            .allocator = allocator,
            .root_node = root_node,
            .root_handle = nh.fromPtr(XmlNode, root_node),
            .virtual_nodes = .{},
        };
    }

    pub fn deinit(self: *XmlAdapter) void {
        self.virtual_nodes.deinit(self.allocator);
    }

    pub fn root(self: *XmlAdapter) NodeRef {
        return self.root_handle;
    }

    // ── Synthetic output nodes ──────────────────────────────────────

    pub fn nodeFromValue(self: *XmlAdapter, val: item.Value) NodeRef {
        return switch (val) {
            .quantity => |q| self.addVirtualNode(.{ .value_quantity = q }),
            .typeInfo => |ti| self.addVirtualNode(.{ .value_typeinfo = ti }),
            else => self.addVirtualNode(.{ .value_scalar = val }),
        };
    }

    pub fn itemNode(self: *XmlAdapter, it: item.Item) NodeRef {
        if (it.data_kind == .node_ref) {
            if (it.node) |n| return n;
        }
        if (it.data_kind == .value) {
            if (it.value) |v| return self.nodeFromValue(v);
        }
        return self.addVirtualNode(.{ .value_scalar = .{ .empty = {} } });
    }

    fn addVirtualNode(self: *XmlAdapter, vn: VirtualNode) NodeRef {
        self.virtual_nodes.append(self.allocator, vn) catch unreachable;
        return nh.fromIndex(self.virtual_nodes.items.len - 1);
    }

    inline fn xmlPtr(handle: NodeRef) *const XmlNode {
        return nh.toPtr(XmlNode, handle);
    }

    inline fn virtualNode(self: *XmlAdapter, handle: NodeRef) VirtualNode {
        return self.virtual_nodes.items[nh.toIndex(handle)];
    }

    fn ptrHandle(v: *const XmlNode) NodeRef {
        return nh.fromPtr(XmlNode, v);
    }

    // ── Helper: classify an XmlNode ────────────────────────────────

    /// Determine the node.Kind for a real XmlNode.
    /// - Has children (excluding extension/id on primitives context) → .object
    /// - Has `value` attribute → scalar kind (inferred or .string)
    /// - Is a `div` element → .string (XHTML)
    /// - Otherwise → .object (complex type with no value)
    fn xmlNodeKind(xml_node: *const XmlNode) node.Kind {
        // XHTML div
        if (std.mem.eql(u8, xml_node.tag, "div")) {
            return .string;
        }

        // If it has a `value` attribute, it's a primitive element.
        // The kind depends on FHIR type, but we treat them all as .string
        // since the evaluator will do type conversion via schema.
        // However, "true"/"false" we report as .bool, and numeric-looking values as .number.
        if (getValueAttr(xml_node)) |val| {
            // Boolean
            if (std.mem.eql(u8, val, "true") or std.mem.eql(u8, val, "false")) {
                return .bool;
            }
            return .string;
        }

        // Complex element → object
        return .object;
    }

    fn getValueAttr(xml_node: *const XmlNode) ?[]const u8 {
        for (xml_node.attributes) |attr| {
            if (std.mem.eql(u8, attr.name, "value")) {
                return attr.value;
            }
        }
        return null;
    }

    /// Count how many children have a given tag name.
    fn countChildrenWithTag(xml_node: *const XmlNode, tag: []const u8) usize {
        var count: usize = 0;
        for (xml_node.children) |child| {
            if (std.mem.eql(u8, child.tag, tag)) count += 1;
        }
        return count;
    }

    /// Get the i-th child with a given tag.
    fn nthChildWithTag(xml_node: *const XmlNode, tag: []const u8, n: usize) ?*const XmlNode {
        var count: usize = 0;
        for (xml_node.children) |child| {
            if (std.mem.eql(u8, child.tag, tag)) {
                if (count == n) return child;
                count += 1;
            }
        }
        return null;
    }

    /// Get distinct child tag names in order of first appearance.
    fn distinctChildTags(self: *XmlAdapter, xml_node: *const XmlNode) []const []const u8 {
        if (xml_node.children.len == 0) return &.{};
        var tags = std.ArrayListUnmanaged([]const u8){};
        for (xml_node.children) |child| {
            var found = false;
            for (tags.items) |existing| {
                if (std.mem.eql(u8, existing, child.tag)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                tags.append(self.allocator, child.tag) catch {};
            }
        }
        return tags.toOwnedSlice(self.allocator) catch &.{};
    }

    /// Resolve a handle to an XmlNode, handling contained_resource virtual nodes.
    fn resolveXmlNode(self: *XmlAdapter, ref: NodeRef) ?*const XmlNode {
        if (!nh.isIndex(ref)) return xmlPtr(ref);
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .contained_resource => |cr| cr,
            else => null,
        };
    }

    // ── Adapter contract: kind ──────────────────────────────────────

    pub fn kind(self: *XmlAdapter, ref: NodeRef) node.Kind {
        if (!nh.isIndex(ref)) {
            return xmlNodeKind(xmlPtr(ref));
        }
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .child_array => .array,
            .resource_type_string => .string,
            .contained_resource => .object,
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

    // ── Adapter contract: objectGet ─────────────────────────────────

    pub fn objectGet(self: *XmlAdapter, ref: NodeRef, key: []const u8) ?NodeRef {
        // Handle virtual nodes first
        if (nh.isIndex(ref)) {
            const vn = self.virtualNode(ref);
            return switch (vn) {
                .contained_resource => |cr| self.xmlObjectGet(cr, key),
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
                .child_array, .resource_type_string, .value_scalar => return null,
            };
        }

        const xml_node = xmlPtr(ref);
        return self.xmlObjectGet(xml_node, key);
    }

    fn xmlObjectGet(self: *XmlAdapter, xml_node: *const XmlNode, key: []const u8) ?NodeRef {
        // Special case: resourceType → tag name of this element
        if (std.mem.eql(u8, key, "resourceType")) {
            return self.addVirtualNode(.{ .resource_type_string = xml_node.tag });
        }

        // For primitives with value attr: support .value, .id, .extension access
        if (getValueAttr(xml_node) != null or std.mem.eql(u8, xml_node.tag, "div")) {
            if (std.mem.eql(u8, key, "value")) {
                // "value" on a primitive returns the value attribute as a scalar
                // But we don't create a separate node — the primitive IS the value.
                // The evaluator accesses this via string()/boolean()/numberText().
                return null;
            }
            // id and extension are child elements even on primitives
        }

        // Look for child elements with this tag name
        const count = countChildrenWithTag(xml_node, key);
        if (count == 0) return null;

        if (count == 1) {
            // Single child: return it directly
            const child = nthChildWithTag(xml_node, key, 0).?;
            // Special case: <contained> wraps a resource element
            if (std.mem.eql(u8, key, "contained")) {
                if (child.children.len == 1) {
                    // Return the inner resource element wrapped
                    return self.addVirtualNode(.{ .contained_resource = child.children[0] });
                }
            }
            return ptrHandle(child);
        }

        // Multiple children with same tag: return a virtual array
        // Special case: <contained> with multiple entries
        if (std.mem.eql(u8, key, "contained")) {
            return self.addVirtualNode(.{ .child_array = .{
                .parent = xml_node,
                .tag = key,
            } });
        }
        return self.addVirtualNode(.{ .child_array = .{
            .parent = xml_node,
            .tag = key,
        } });
    }

    // ── Adapter contract: objectCount ───────────────────────────────

    pub fn objectCount(self: *XmlAdapter, ref: NodeRef) usize {
        const xml_node = self.resolveXmlNode(ref) orelse {
            if (nh.isIndex(ref)) {
                const vn = self.virtualNode(ref);
                return switch (vn) {
                    .value_quantity => 2,
                    .value_typeinfo => 2,
                    else => 0,
                };
            }
            return 0;
        };
        return self.xmlObjectCount(xml_node);
    }

    fn xmlObjectCount(self: *XmlAdapter, xml_node: *const XmlNode) usize {
        const tags = self.distinctChildTags(xml_node);
        return tags.len;
    }

    // ── Adapter contract: objectIter ────────────────────────────────

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

    pub fn objectIter(self: *XmlAdapter, ref: NodeRef) ObjectIter {
        const xml_node = self.resolveXmlNode(ref) orelse {
            if (nh.isIndex(ref)) {
                const vn = self.virtualNode(ref);
                return switch (vn) {
                    .value_quantity => |q| self.syntheticQuantityIter(q),
                    .value_typeinfo => |ti| self.syntheticTypeInfoIter(ti),
                    else => .{ .entries = &.{}, .idx = 0 },
                };
            }
            return .{ .entries = &.{}, .idx = 0 };
        };
        return self.xmlObjectIter(xml_node);
    }

    fn xmlObjectIter(self: *XmlAdapter, xml_node: *const XmlNode) ObjectIter {
        const tags = self.distinctChildTags(xml_node);
        if (tags.len == 0) return .{ .entries = &.{}, .idx = 0 };

        const entries = self.allocator.alloc(node.ObjectEntry(NodeRef), tags.len) catch
            return .{ .entries = &.{}, .idx = 0 };

        for (tags, 0..) |tag, i| {
            const child_ref = self.xmlObjectGet(xml_node, tag) orelse continue;
            entries[i] = .{ .key = tag, .value = child_ref };
        }
        return .{ .entries = entries, .idx = 0 };
    }

    fn syntheticQuantityIter(self: *XmlAdapter, q: item.Quantity) ObjectIter {
        const entries = self.allocator.alloc(node.ObjectEntry(NodeRef), 2) catch
            return .{ .entries = &.{}, .idx = 0 };
        entries[0] = .{ .key = "value", .value = self.addVirtualNode(.{ .value_scalar = .{ .decimal = q.value } }) };
        entries[1] = .{ .key = "unit", .value = self.addVirtualNode(.{ .value_scalar = .{ .string = q.unit } }) };
        return .{ .entries = entries, .idx = 0 };
    }

    fn syntheticTypeInfoIter(self: *XmlAdapter, ti: item.TypeInfo) ObjectIter {
        const entries = self.allocator.alloc(node.ObjectEntry(NodeRef), 2) catch
            return .{ .entries = &.{}, .idx = 0 };
        entries[0] = .{ .key = "namespace", .value = self.addVirtualNode(.{ .value_scalar = .{ .string = ti.namespace } }) };
        entries[1] = .{ .key = "name", .value = self.addVirtualNode(.{ .value_scalar = .{ .string = ti.name } }) };
        return .{ .entries = entries, .idx = 0 };
    }

    // ── Adapter contract: arrayLen / arrayAt ────────────────────────

    pub fn arrayLen(self: *XmlAdapter, ref: NodeRef) usize {
        if (!nh.isIndex(ref)) return 0; // Real XML nodes are never arrays
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .child_array => |ca| countChildrenWithTag(ca.parent, ca.tag),
            else => 0,
        };
    }

    pub fn arrayAt(self: *XmlAdapter, ref: NodeRef, idx: usize) NodeRef {
        if (!nh.isIndex(ref)) unreachable;
        const vn = self.virtualNode(ref);
        switch (vn) {
            .child_array => |ca| {
                const child = nthChildWithTag(ca.parent, ca.tag, idx).?;
                // For contained resources, unwrap
                if (std.mem.eql(u8, ca.tag, "contained")) {
                    if (child.children.len == 1) {
                        return self.addVirtualNode(.{ .contained_resource = child.children[0] });
                    }
                }
                return ptrHandle(child);
            },
            else => unreachable,
        }
    }

    // ── Adapter contract: scalar accessors ──────────────────────────

    pub fn string(self: *XmlAdapter, ref: NodeRef) []const u8 {
        if (!nh.isIndex(ref)) {
            const xml_node = xmlPtr(ref);
            // XHTML div: serialize to string
            if (std.mem.eql(u8, xml_node.tag, "div")) {
                return xml_parser.serializeNode(self.allocator, xml_node) catch "";
            }
            // Primitive: value attribute
            return getValueAttr(xml_node) orelse "";
        }
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .resource_type_string => |s| s,
            .contained_resource => "",
            .child_array => "",
            .value_scalar => |val| switch (val) {
                .string, .date, .time, .dateTime => |s| s,
                .decimal => |s| s,
                else => "",
            },
            .value_quantity => "",
            .value_typeinfo => "",
        };
    }

    pub fn numberText(self: *XmlAdapter, ref: NodeRef) []const u8 {
        if (nh.isIndex(ref)) {
            const vn = self.virtualNode(ref);
            switch (vn) {
                .value_scalar => |val| switch (val) {
                    .integer => |i| return std.fmt.allocPrint(self.allocator, "{d}", .{i}) catch "0",
                    .long => |l| return std.fmt.allocPrint(self.allocator, "{d}", .{l}) catch "0",
                    .decimal => |s| return s,
                    else => return "0",
                },
                .value_quantity => |q| return q.value,
                else => {},
            }
        }
        // For real XML nodes, the value attribute IS the number text
        if (!nh.isIndex(ref)) {
            const xml_node = xmlPtr(ref);
            return getValueAttr(xml_node) orelse "0";
        }
        return "0";
    }

    pub fn boolean(self: *XmlAdapter, ref: NodeRef) bool {
        if (!nh.isIndex(ref)) {
            const xml_node = xmlPtr(ref);
            const val = getValueAttr(xml_node) orelse return false;
            return std.mem.eql(u8, val, "true");
        }
        const vn = self.virtualNode(ref);
        return switch (vn) {
            .value_scalar => |val| switch (val) {
                .boolean => |b| b,
                else => false,
            },
            else => false,
        };
    }

    // ── nodeToJsonValue — for resolveResult serialization ───────────

    pub fn nodeToJsonValue(
        self: *XmlAdapter,
        allocator: std.mem.Allocator,
        ref: NodeRef,
    ) !std.json.Value {
        if (!nh.isIndex(ref)) {
            return self.xmlNodeToJsonValue(allocator, xmlPtr(ref));
        }
        const vn = self.virtualNode(ref);
        switch (vn) {
            .contained_resource => |cr| {
                return self.xmlNodeToJsonValue(allocator, cr);
            },
            .child_array => |ca| {
                var arr = std.ArrayListUnmanaged(std.json.Value){};
                const len = countChildrenWithTag(ca.parent, ca.tag);
                for (0..len) |i| {
                    const child = nthChildWithTag(ca.parent, ca.tag, i).?;
                    const val = try self.xmlNodeToJsonValue(allocator, child);
                    try arr.append(allocator, val);
                }
                const owned = try arr.toOwnedSlice(allocator);
                return .{ .array = .{ .items = owned, .capacity = owned.len, .allocator = allocator } };
            },
            .resource_type_string => |s| return .{ .string = s },
            .value_scalar => |val| return syntheticValueToJson(val),
            .value_quantity => |q| {
                var obj = std.json.ObjectMap.init(allocator);
                try obj.put("value", .{ .number_string = q.value });
                try obj.put("unit", .{ .string = q.unit });
                return .{ .object = obj };
            },
            .value_typeinfo => |ti| {
                var obj = std.json.ObjectMap.init(allocator);
                try obj.put("namespace", .{ .string = ti.namespace });
                try obj.put("name", .{ .string = ti.name });
                return .{ .object = obj };
            },
        }
    }

    fn xmlNodeToJsonValue(self: *XmlAdapter, allocator: std.mem.Allocator, xml_node: *const XmlNode) !std.json.Value {
        const k = xmlNodeKind(xml_node);
        return switch (k) {
            .string => .{ .string = self.string(ptrHandle(xml_node)) },
            .bool => .{ .bool = std.mem.eql(u8, getValueAttr(xml_node) orelse "false", "true") },
            .number => .{ .number_string = getValueAttr(xml_node) orelse "0" },
            .null => .{ .null = {} },
            .object => {
                var obj = std.json.ObjectMap.init(allocator);
                // Add resourceType
                try obj.put("resourceType", .{ .string = xml_node.tag });
                // Add children
                const tags = self.distinctChildTags(xml_node);
                for (tags) |tag| {
                    const count = countChildrenWithTag(xml_node, tag);
                    if (count == 1) {
                        const child = nthChildWithTag(xml_node, tag, 0).?;
                        const val = try self.xmlNodeToJsonValue(allocator, child);
                        try obj.put(tag, val);
                    } else {
                        var arr = std.ArrayListUnmanaged(std.json.Value){};
                        for (0..count) |i| {
                            const child = nthChildWithTag(xml_node, tag, i).?;
                            const val = try self.xmlNodeToJsonValue(allocator, child);
                            try arr.append(allocator, val);
                        }
                        const owned = try arr.toOwnedSlice(allocator);
                        try obj.put(tag, .{ .array = .{ .items = owned, .capacity = owned.len, .allocator = allocator } });
                    }
                }
                return .{ .object = obj };
            },
            .array => .{ .null = {} }, // shouldn't happen for real XML nodes
        };
    }
};

fn syntheticValueToJson(val: item.Value) std.json.Value {
    return switch (val) {
        .empty => .{ .null = {} },
        .boolean => |b| .{ .bool = b },
        .integer, .long => |i| .{ .integer = i },
        .decimal => |s| .{ .number_string = s },
        .string, .date, .time, .dateTime => |s| .{ .string = s },
        .quantity => .{ .null = {} },
        .typeInfo => .{ .null = {} },
    };
}

// Verify XmlAdapter conforms to NodeAdapter interface
comptime {
    node.requireAdapter(XmlAdapter);
}

// ── Tests ────────────────────────────────────────────────────────────

test "xml adapter: simple patient" {
    const alloc = std.testing.allocator;
    const xml_text = "<Patient><id value=\"123\"/><active value=\"true\"/><birthDate value=\"1974-12-25\"/></Patient>";
    const doc = try xml_parser.parse(alloc, xml_text);
    defer alloc.destroy(doc);
    defer {
        for (doc.children) |child| {
            alloc.free(child.attributes);
            alloc.destroy(child);
        }
        alloc.free(doc.children);
    }

    var adapter = XmlAdapter.init(alloc, doc);
    defer adapter.deinit();

    const root_ref = adapter.root();

    // kind should be object
    try std.testing.expectEqual(node.Kind.object, adapter.kind(root_ref));

    // resourceType
    const rt_ref = adapter.objectGet(root_ref, "resourceType").?;
    try std.testing.expectEqual(node.Kind.string, adapter.kind(rt_ref));
    try std.testing.expectEqualStrings("Patient", adapter.string(rt_ref));

    // id
    const id_ref = adapter.objectGet(root_ref, "id").?;
    try std.testing.expectEqualStrings("123", adapter.string(id_ref));

    // active (boolean)
    const active_ref = adapter.objectGet(root_ref, "active").?;
    try std.testing.expectEqual(node.Kind.bool, adapter.kind(active_ref));
    try std.testing.expect(adapter.boolean(active_ref));

    // birthDate
    const bd_ref = adapter.objectGet(root_ref, "birthDate").?;
    try std.testing.expectEqualStrings("1974-12-25", adapter.string(bd_ref));
}

test "xml adapter: repeated elements as array" {
    const alloc = std.testing.allocator;
    const xml_text =
        \\<Patient>
        \\  <name><family value="Smith"/></name>
        \\  <name><family value="Jones"/></name>
        \\</Patient>
    ;
    const doc = try xml_parser.parse(alloc, xml_text);
    // Simplified cleanup - in real usage this is arena-allocated
    defer {
        for (doc.children) |name_node| {
            for (name_node.children) |family_node| {
                alloc.free(family_node.attributes);
                alloc.destroy(family_node);
            }
            alloc.free(name_node.children);
            alloc.destroy(name_node);
        }
        alloc.free(doc.children);
        alloc.destroy(doc);
    }

    var adapter = XmlAdapter.init(alloc, doc);
    defer adapter.deinit();

    const root_ref = adapter.root();
    const name_ref = adapter.objectGet(root_ref, "name").?;

    // Should be an array
    try std.testing.expectEqual(node.Kind.array, adapter.kind(name_ref));
    try std.testing.expectEqual(@as(usize, 2), adapter.arrayLen(name_ref));

    // Access first name
    const name0 = adapter.arrayAt(name_ref, 0);
    const family0 = adapter.objectGet(name0, "family").?;
    try std.testing.expectEqualStrings("Smith", adapter.string(family0));

    // Access second name
    const name1 = adapter.arrayAt(name_ref, 1);
    const family1 = adapter.objectGet(name1, "family").?;
    try std.testing.expectEqualStrings("Jones", adapter.string(family1));
}

test "xml adapter: object iteration" {
    const alloc = std.testing.allocator;
    const xml_text = "<Patient><id value=\"123\"/><active value=\"true\"/></Patient>";
    const doc = try xml_parser.parse(alloc, xml_text);
    defer {
        for (doc.children) |child| {
            alloc.free(child.attributes);
            alloc.destroy(child);
        }
        alloc.free(doc.children);
        alloc.destroy(doc);
    }

    var adapter = XmlAdapter.init(alloc, doc);
    defer adapter.deinit();

    const root_ref = adapter.root();
    var iter = adapter.objectIter(root_ref);
    defer alloc.free(iter.entries);

    const entry0 = iter.next().?;
    try std.testing.expectEqualStrings("id", entry0.key);

    const entry1 = iter.next().?;
    try std.testing.expectEqualStrings("active", entry1.key);

    try std.testing.expectEqual(@as(?node.ObjectEntry(XmlAdapter.NodeRef), null), iter.next());
}
