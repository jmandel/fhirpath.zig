//! JsonDoc adapter - our custom DOM with span preservation.
//!
//! This adapter wraps the existing jsondoc.JsonDoc and implements the NodeAdapter interface.
//! Key benefits:
//! - Zero-copy spans: can return slices of the original input buffer
//! - Direct node indexing without pointer chasing
//! - Arena-based allocation for predictable memory

const std = @import("std");
const jsondoc = @import("../jsondoc.zig");
const node = @import("../node.zig");

/// Object field entry for iteration
pub const ObjectEntry = node.ObjectEntry(NodeIndex);

pub const NodeIndex = jsondoc.NodeIndex;

pub const JsonDocAdapter = struct {
    doc: *jsondoc.JsonDoc,

    pub const NodeRef = NodeIndex;

    pub fn init(doc: *jsondoc.JsonDoc) JsonDocAdapter {
        return .{ .doc = doc };
    }

    pub fn root(self: *JsonDocAdapter) NodeRef {
        return self.doc.root;
    }

    pub fn kind(self: *JsonDocAdapter, ref: NodeRef) node.Kind {
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

    pub fn objectGet(self: *JsonDocAdapter, ref: NodeRef, key: []const u8) ?NodeRef {
        const n = self.doc.node(ref);
        if (n.kind != .object) return null;
        for (n.data.object) |field| {
            if (std.mem.eql(u8, field.key, key)) {
                return field.value;
            }
        }
        return null;
    }

    pub fn objectCount(self: *JsonDocAdapter, ref: NodeRef) usize {
        const n = self.doc.node(ref);
        if (n.kind != .object) return 0;
        return n.data.object.len;
    }

    pub const ObjectIter = struct {
        fields: []const jsondoc.Field,
        index: usize,

        pub fn next(self: *ObjectIter) ?ObjectEntry {
            if (self.index >= self.fields.len) return null;
            const f = self.fields[self.index];
            self.index += 1;
            return .{ .key = f.key, .value = f.value };
        }
    };

    pub fn objectIter(self: *JsonDocAdapter, ref: NodeRef) ObjectIter {
        const n = self.doc.node(ref);
        if (n.kind != .object) return .{ .fields = &[_]jsondoc.Field{}, .index = 0 };
        return .{ .fields = n.data.object, .index = 0 };
    }

    pub fn arrayLen(self: *JsonDocAdapter, ref: NodeRef) usize {
        const n = self.doc.node(ref);
        if (n.kind != .array) return 0;
        return n.data.array.len;
    }

    pub fn arrayAt(self: *JsonDocAdapter, ref: NodeRef, idx: usize) NodeRef {
        const n = self.doc.node(ref);
        return n.data.array[idx];
    }

    pub fn string(self: *JsonDocAdapter, ref: NodeRef) []const u8 {
        const n = self.doc.node(ref);
        return n.data.string;
    }

    pub fn numberText(self: *JsonDocAdapter, ref: NodeRef) []const u8 {
        const n = self.doc.node(ref);
        return n.data.number;
    }

    pub fn boolean(self: *JsonDocAdapter, ref: NodeRef) bool {
        const n = self.doc.node(ref);
        return n.data.bool;
    }

    // Optional: span support for zero-copy JSON slices
    pub fn span(self: *JsonDocAdapter, ref: NodeRef) node.Span {
        const n = self.doc.node(ref);
        return .{ .pos = n.start, .end = n.end };
    }

    // Optional: stringify by returning the original JSON text
    pub fn stringify(self: *JsonDocAdapter, ref: NodeRef) []const u8 {
        const n = self.doc.node(ref);
        return self.doc.text[n.start..n.end];
    }
};

// Verify JsonDocAdapter conforms to NodeAdapter interface
comptime {
    node.requireAdapter(JsonDocAdapter);
}
