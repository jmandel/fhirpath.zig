//! JsonDoc adapter with explicit inline hints
const std = @import("std");
const jsondoc = @import("../jsondoc.zig");
const node = @import("../node.zig");

pub const ObjectEntry = node.ObjectEntry(NodeIndex);
pub const NodeIndex = jsondoc.NodeIndex;

pub const JsonDocAdapter = struct {
    doc: *jsondoc.JsonDoc,

    pub const NodeRef = NodeIndex;

    pub inline fn init(doc: *jsondoc.JsonDoc) JsonDocAdapter {
        return .{ .doc = doc };
    }

    pub inline fn root(self: *JsonDocAdapter) NodeRef {
        return self.doc.root;
    }

    pub inline fn kind(self: *JsonDocAdapter, ref: NodeRef) node.Kind {
        const n = self.doc.node(ref);
        return switch (n.kind) {
            .object => .object,
            .array => .array,
            .string => .string,
            .number => .number,
            .bool => .bool,
            .null => .null,
        };
    }

    pub inline fn objectGet(self: *JsonDocAdapter, ref: NodeRef, key: []const u8) ?NodeRef {
        const n = self.doc.node(ref);
        if (n.kind != .object) return null;
        for (n.data.object) |field| {
            if (std.mem.eql(u8, field.key, key)) {
                return field.value;
            }
        }
        return null;
    }

    pub inline fn objectCount(self: *JsonDocAdapter, ref: NodeRef) usize {
        const n = self.doc.node(ref);
        if (n.kind != .object) return 0;
        return n.data.object.len;
    }

    pub const ObjectIter = struct {
        fields: []const jsondoc.Field,
        index: usize,

        pub inline fn next(self: *ObjectIter) ?ObjectEntry {
            if (self.index >= self.fields.len) return null;
            const f = self.fields[self.index];
            self.index += 1;
            return .{ .key = f.key, .value = f.value };
        }
    };

    pub inline fn objectIter(self: *JsonDocAdapter, ref: NodeRef) ObjectIter {
        const n = self.doc.node(ref);
        if (n.kind != .object) return .{ .fields = &[_]jsondoc.Field{}, .index = 0 };
        return .{ .fields = n.data.object, .index = 0 };
    }

    pub inline fn arrayLen(self: *JsonDocAdapter, ref: NodeRef) usize {
        const n = self.doc.node(ref);
        if (n.kind != .array) return 0;
        return n.data.array.len;
    }

    pub inline fn arrayAt(self: *JsonDocAdapter, ref: NodeRef, idx: usize) NodeRef {
        const n = self.doc.node(ref);
        return n.data.array[idx];
    }

    pub inline fn string(self: *JsonDocAdapter, ref: NodeRef) []const u8 {
        const n = self.doc.node(ref);
        return n.data.string;
    }

    pub inline fn numberText(self: *JsonDocAdapter, ref: NodeRef) []const u8 {
        const n = self.doc.node(ref);
        return n.data.number;
    }

    pub inline fn boolean(self: *JsonDocAdapter, ref: NodeRef) bool {
        const n = self.doc.node(ref);
        return n.data.bool;
    }

    pub inline fn span(self: *JsonDocAdapter, ref: NodeRef) node.Span {
        const n = self.doc.node(ref);
        return .{ .pos = n.start, .end = n.end };
    }

    pub inline fn stringify(self: *JsonDocAdapter, ref: NodeRef) []const u8 {
        const n = self.doc.node(ref);
        return self.doc.text[n.start..n.end];
    }
};

comptime {
    node.requireAdapter(JsonDocAdapter);
}
