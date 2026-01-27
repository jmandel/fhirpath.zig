const std = @import("std");
const jsondoc = @import("jsondoc.zig");
const item = @import("item.zig");

pub fn itemToJsonValue(
    allocator: std.mem.Allocator,
    doc: *jsondoc.JsonDoc,
    it: item.Item,
) !std.json.Value {
    if (it.data_kind == .value and it.value != null) {
        return valueToJson(allocator, it.value.?);
    }
    if (it.data_kind == .json_span and it.node != null) {
        return nodeToJsonValue(allocator, doc, @intCast(it.node.?));
    }
    return std.json.Value{ .null = {} };
}

fn valueToJson(allocator: std.mem.Allocator, v: item.Value) !std.json.Value {
    return switch (v) {
        .empty => std.json.Value{ .null = {} },
        .boolean => |b| std.json.Value{ .bool = b },
        .integer => |i| std.json.Value{ .integer = i },
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
    };
}

pub fn nodeToJsonValue(
    allocator: std.mem.Allocator,
    doc: *jsondoc.JsonDoc,
    idx: jsondoc.NodeIndex,
) !std.json.Value {
    const node = doc.node(idx).*;
    return switch (node.kind) {
        .null => std.json.Value{ .null = {} },
        .bool => std.json.Value{ .bool = node.data.bool },
        .number => parseNumber(node.data.number),
        .string => std.json.Value{ .string = node.data.string },
        .array => blk: {
            var arr = std.ArrayListUnmanaged(std.json.Value){};
            for (node.data.array) |child| {
                const val = try nodeToJsonValue(allocator, doc, child);
                try arr.append(allocator, val);
            }
            const owned = try arr.toOwnedSlice(allocator);
            break :blk std.json.Value{ .array = std.json.Array{ .items = owned, .capacity = owned.len, .allocator = allocator } };
        },
        .object => blk: {
            var obj = std.json.ObjectMap.init(allocator);
            for (node.data.object) |field| {
                const val = try nodeToJsonValue(allocator, doc, field.value);
                try obj.put(field.key, val);
            }
            break :blk std.json.Value{ .object = obj };
        },
    };
}

fn parseNumber(text: []const u8) std.json.Value {
    if (std.mem.indexOfScalar(u8, text, '.') != null or std.mem.indexOfAny(u8, text, "eE") != null) {
        const value = std.fmt.parseFloat(f64, text) catch {
            return .{ .number_string = text };
        };
        return .{ .float = value };
    }
    const value = std.fmt.parseInt(i64, text, 10) catch {
        return .{ .number_string = text };
    };
    return .{ .integer = value };
}

pub fn itemToTypedJsonValue(
    allocator: std.mem.Allocator,
    doc: *jsondoc.JsonDoc,
    it: item.Item,
    type_name: []const u8,
) !std.json.Value {
    var obj = std.json.ObjectMap.init(allocator);
    
    // Add type
    try obj.put("type", .{ .string = type_name });
    
    // Add value
    const val = try itemToJsonValue(allocator, doc, it);
    try obj.put("value", val);
    
    return .{ .object = obj };
}
