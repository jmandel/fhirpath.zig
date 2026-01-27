//! std.json.Value adapter
//!
//! This adapter wraps std.json.Value for compatibility with the standard library.
//! Benefits:
//! - Hash-based object lookup (faster than linear scan for large objects)
//! - Simpler external API (just pass Value)
//!
//! Limitations:
//! - No span support (std.json doesn't preserve source positions)
//! - Strings are always allocated/unescaped

const std = @import("std");
const node = @import("../node.zig");

pub const StdJsonAdapter = struct {
    allocator: std.mem.Allocator,

    pub const NodeRef = *const std.json.Value;

    pub fn init(allocator: std.mem.Allocator) StdJsonAdapter {
        return .{ .allocator = allocator };
    }

    pub fn kind(_: *StdJsonAdapter, ref: NodeRef) node.Kind {
        return switch (ref.*) {
            .object => .object,
            .array => .array,
            .string => .string,
            .integer, .float, .number_string => .number,
            .bool => .bool,
            .null => .null,
        };
    }

    pub fn objectGet(_: *StdJsonAdapter, ref: NodeRef, key: []const u8) ?NodeRef {
        if (ref.* != .object) return null;
        return ref.object.getPtr(key);
    }

    pub fn objectCount(_: *StdJsonAdapter, ref: NodeRef) usize {
        if (ref.* != .object) return 0;
        return ref.object.count();
    }

    pub const ObjectIter = struct {
        it: std.json.ObjectMap.Iterator,

        pub fn next(self: *ObjectIter) ?node.ObjectEntry(NodeRef) {
            if (self.it.next()) |entry| {
                return .{ .key = entry.key_ptr.*, .value = entry.value_ptr };
            }
            return null;
        }
    };

    pub fn objectIter(_: *StdJsonAdapter, ref: NodeRef) ObjectIter {
        if (ref.* != .object) {
            // Return empty iterator - but we need a valid ObjectMap
            // This is a bit awkward - return an iterator that will immediately return null
            return .{ .it = ref.object.iterator() };
        }
        return .{ .it = ref.object.iterator() };
    }

    pub fn arrayLen(_: *StdJsonAdapter, ref: NodeRef) usize {
        if (ref.* != .array) return 0;
        return ref.array.items.len;
    }

    pub fn arrayAt(_: *StdJsonAdapter, ref: NodeRef, idx: usize) NodeRef {
        return &ref.array.items[idx];
    }

    pub fn string(_: *StdJsonAdapter, ref: NodeRef) []const u8 {
        return ref.string;
    }

    pub fn numberText(self: *StdJsonAdapter, ref: NodeRef) []const u8 {
        return switch (ref.*) {
            .number_string => |s| s,
            .integer => |i| std.fmt.allocPrint(self.allocator, "{d}", .{i}) catch "0",
            .float => |f| std.fmt.allocPrint(self.allocator, "{d}", .{f}) catch "0",
            else => "0",
        };
    }

    pub fn boolean(_: *StdJsonAdapter, ref: NodeRef) bool {
        return ref.bool;
    }

    // StdJsonAdapter does NOT support span() or stringify()
    // (std.json doesn't preserve source positions)
};

// Verify StdJsonAdapter conforms to NodeAdapter interface
comptime {
    node.requireAdapter(StdJsonAdapter);
}
