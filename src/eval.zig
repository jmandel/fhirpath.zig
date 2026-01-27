const std = @import("std");
const ast = @import("ast.zig");

pub const ValueList = std.ArrayList(std.json.Value);

pub const EvalResult = struct {
    values: ValueList,
    parsed: std.json.Parsed(std.json.Value),

    pub fn deinit(self: *EvalResult) void {
        self.values.deinit();
        self.parsed.deinit();
    }
};

pub const Env = struct {
    map: std.StringHashMap(std.json.Value),

    pub fn init(allocator: std.mem.Allocator) Env {
        return .{ .map = std.StringHashMap(std.json.Value).init(allocator) };
    }

    pub fn deinit(self: *Env) void {
        self.map.deinit();
    }

    pub fn put(self: *Env, name: []const u8, value: std.json.Value) !void {
        try self.map.put(name, value);
    }
};

pub fn evalWithJson(
    allocator: std.mem.Allocator,
    expr: ast.Expr,
    json_text: []const u8,
    env: ?*Env,
) !EvalResult {
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_text, .{});
    const values = try evalExpression(allocator, expr, parsed.value, env);
    return .{ .values = values, .parsed = parsed };
}

pub fn evalExpression(
    allocator: std.mem.Allocator,
    expr: ast.Expr,
    root: std.json.Value,
    env: ?*Env,
) !ValueList {
    switch (expr) {
        .Path => |p| return evalPath(allocator, p, root, env),
        .Env => |name| {
            var out = ValueList.empty;
            if (env) |e| {
                if (e.map.get(name)) |val| {
                    try out.append(allocator, val);
                }
            }
            return out;
        },
        .Literal => |lit| {
            var out = ValueList.empty;
            try out.append(allocator, literalToJson(lit));
            return out;
        },
    }
}

fn evalPath(
    allocator: std.mem.Allocator,
    path: ast.PathExpr,
    root: std.json.Value,
    env: ?*Env,
) !ValueList {
    var current = ValueList.empty;
    defer current.deinit(allocator);

    switch (path.root) {
        .This => try current.append(allocator, root),
        .Env => |name| {
            if (env) |e| {
                if (e.map.get(name)) |val| {
                    try current.append(allocator, val);
                }
            }
        },
    }

    for (path.segments) |seg| {
        var next = ValueList.empty;
        defer next.deinit(allocator);
        for (current.items) |item| {
            try applySegment(item, seg, &next, allocator);
        }
        current.clearAndFree(allocator);
        try current.appendSlice(allocator, next.items);
    }

    var result = ValueList.empty;
    try result.appendSlice(allocator, current.items);
    return result;
}

fn applySegment(
    value: std.json.Value,
    name: []const u8,
    out: *ValueList,
    allocator: std.mem.Allocator,
) !void {
    switch (value) {
        .object => |obj| {
            if (obj.get(name)) |child| {
                switch (child) {
                    .array => |arr| {
                        for (arr.items) |item| {
                            try out.append(allocator, item);
                        }
                    },
                    else => try out.append(allocator, child),
                }
            }
        },
        .array => |arr| {
            for (arr.items) |child| {
                try applySegment(child, name, out, allocator);
            }
        },
        else => {},
    }
}

fn literalToJson(lit: ast.Literal) std.json.Value {
    return switch (lit) {
        .Null => .null,
        .Bool => |b| .{ .bool = b },
        .String => |s| .{ .string = s },
        .Number => |n| .{ .string = n }, // TODO: parse numeric
        .Date => |d| .{ .string = d },
        .DateTime => |d| .{ .string = d },
        .Time => |t| .{ .string = t },
    };
}
