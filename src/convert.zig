const std = @import("std");
const node_mod = @import("node.zig");
const item = @import("item.zig");

pub fn valueToJsonValue(allocator: std.mem.Allocator, v: item.Value) !std.json.Value {
    return switch (v) {
        .empty => std.json.Value{ .null = {} },
        .boolean => |b| std.json.Value{ .bool = b },
        .integer => |i| std.json.Value{ .integer = i },
        .long => |i| std.json.Value{ .integer = i },
        .decimal => |s| std.json.Value{ .number_string = s },
        .string => |s| std.json.Value{ .string = s },
        .date => |s| std.json.Value{ .string = s },
        .time => |s| std.json.Value{ .string = s },
        .dateTime => |s| std.json.Value{ .string = s },
        .quantity => |q| blk: {
            var obj = std.json.ObjectMap.init(allocator);
            try obj.put("value", parseNumber(q.value));
            try obj.put("unit", .{ .string = q.unit });
            break :blk std.json.Value{ .object = obj };
        },
        .typeInfo => |ti| blk: {
            var obj = std.json.ObjectMap.init(allocator);
            try obj.put("namespace", .{ .string = ti.namespace });
            try obj.put("name", .{ .string = ti.name });
            break :blk std.json.Value{ .object = obj };
        },
    };
}

fn parseNumber(text: []const u8) std.json.Value {
    // Decimals (containing '.' or scientific notation): preserve as number_string
    // to retain trailing zeros and full precision.
    if (std.mem.indexOfScalar(u8, text, '.') != null or std.mem.indexOfAny(u8, text, "eE") != null) {
        return .{ .number_string = text };
    }
    const value = std.fmt.parseInt(i64, text, 10) catch {
        return .{ .number_string = text };
    };
    return .{ .integer = value };
}

/// Generic adapter-based item to JSON conversion.
/// Works with any adapter type (FhirJsonAdapter, StdJsonAdapter, etc.)
pub fn adapterItemToJsonValue(
    comptime A: type,
    allocator: std.mem.Allocator,
    adapter: *A,
    it: item.Item,
) !std.json.Value {
    if (it.data_kind == .value and it.value != null) {
        return valueToJsonValue(allocator, it.value.?);
    }
    if (it.data_kind == .node_ref and it.node != null) {
        return adapterNodeToJsonValue(A, allocator, adapter, it.node.?);
    }
    return std.json.Value{ .null = {} };
}

/// Generic adapter-based typed item to JSON conversion.
pub fn adapterItemToTypedJsonValue(
    comptime A: type,
    allocator: std.mem.Allocator,
    adapter: *A,
    it: item.Item,
    type_name: []const u8,
) !std.json.Value {
    var obj = std.json.ObjectMap.init(allocator);
    try obj.put("type", .{ .string = type_name });
    const val = try adapterItemToJsonValue(A, allocator, adapter, it);
    try obj.put("value", val);
    return .{ .object = obj };
}

/// Convert an adapter node to std.json.Value by traversing via adapter methods
pub fn adapterNodeToJsonValue(
    comptime A: type,
    allocator: std.mem.Allocator,
    adapter: *A,
    ref: A.NodeRef,
) !std.json.Value {
    switch (A.kind(adapter, ref)) {
        .null => return .{ .null = {} },
        .bool => return .{ .bool = A.boolean(adapter, ref) },
        .number => return parseNumber(A.numberText(adapter, ref)),
        .string => return .{ .string = A.string(adapter, ref) },
        .array => {
            var arr = std.ArrayListUnmanaged(std.json.Value){};
            const len = A.arrayLen(adapter, ref);
            for (0..len) |i| {
                const child_ref = A.arrayAt(adapter, ref, i);
                const val = try adapterNodeToJsonValue(A, allocator, adapter, child_ref);
                try arr.append(allocator, val);
            }
            const owned = try arr.toOwnedSlice(allocator);
            return .{ .array = .{ .items = owned, .capacity = owned.len, .allocator = allocator } };
        },
        .object => {
            var obj = std.json.ObjectMap.init(allocator);
            var iter = A.objectIter(adapter, ref);
            while (iter.next()) |entry| {
                const val = try adapterNodeToJsonValue(A, allocator, adapter, entry.value);
                try obj.put(entry.key, val);
            }
            return .{ .object = obj };
        },
    }
}
