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

const EvalError = error{ InvalidFunction, InvalidPredicate, SingletonRequired } || error{OutOfMemory};

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
) EvalError!ValueList {
    return evalExpressionCtx(allocator, expr, root, env, null);
}

fn evalExpressionCtx(
    allocator: std.mem.Allocator,
    expr: ast.Expr,
    root: std.json.Value,
    env: ?*Env,
    index: ?usize,
) EvalError!ValueList {
    switch (expr) {
        .Path => |p| return evalPath(allocator, p, root, env, index),
        .Binary => |b| return evalBinary(allocator, b, root, env, index),
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
    index: ?usize,
) EvalError!ValueList {
    var current = ValueList.empty;

    switch (path.root) {
        .This => try current.append(allocator, root),
        .Env => |name| {
            if (env) |e| {
                if (e.map.get(name)) |val| {
                    try current.append(allocator, val);
                }
            }
        },
        .Index => {
            if (index) |idx| {
                try current.append(allocator, .{ .integer = @intCast(idx) });
            }
        },
    }

    for (path.steps) |step| {
        switch (step) {
            .Property => |name| {
                var next = ValueList.empty;
                for (current.items) |item| {
                    try applySegment(item, name, &next, allocator);
                }
                current.deinit(allocator);
                current = next;
            },
            .Index => |idx| {
                var next = ValueList.empty;
                if (idx < current.items.len) {
                    try next.append(allocator, current.items[idx]);
                }
                current.deinit(allocator);
                current = next;
            },
            .Function => |call| {
                const next = try evalFunction(allocator, call, current.items, env);
                current.deinit(allocator);
                current = next;
            },
        }
    }

    return current;
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

fn evalBinary(
    allocator: std.mem.Allocator,
    expr: ast.BinaryExpr,
    root: std.json.Value,
    env: ?*Env,
    index: ?usize,
) EvalError!ValueList {
    var left = try evalExpressionCtx(allocator, expr.left.*, root, env, index);
    defer left.deinit(allocator);
    var right = try evalExpressionCtx(allocator, expr.right.*, root, env, index);
    defer right.deinit(allocator);
    return evalEquality(allocator, left.items, right.items);
}

fn evalEquality(
    allocator: std.mem.Allocator,
    left: []const std.json.Value,
    right: []const std.json.Value,
) EvalError!ValueList {
    var out = ValueList.empty;
    if (left.len == 0 or right.len == 0) return out;
    var matched = false;
    for (left) |l| {
        for (right) |r| {
            if (jsonEqual(l, r)) {
                matched = true;
                break;
            }
        }
        if (matched) break;
    }
    try out.append(allocator, .{ .bool = matched });
    return out;
}

fn evalFunction(
    allocator: std.mem.Allocator,
    call: ast.FunctionCall,
    input: []const std.json.Value,
    env: ?*Env,
) EvalError!ValueList {
    if (std.mem.eql(u8, call.name, "count")) {
        var out = ValueList.empty;
        try out.append(allocator, .{ .integer = @intCast(input.len) });
        return out;
    }
    if (std.mem.eql(u8, call.name, "empty")) {
        var out = ValueList.empty;
        try out.append(allocator, .{ .bool = input.len == 0 });
        return out;
    }
    if (std.mem.eql(u8, call.name, "exists")) {
        var out = ValueList.empty;
        try out.append(allocator, .{ .bool = input.len != 0 });
        return out;
    }
    if (std.mem.eql(u8, call.name, "distinct")) {
        return distinctValues(allocator, input);
    }
    if (std.mem.eql(u8, call.name, "isDistinct")) {
        var distinct = try distinctValues(allocator, input);
        defer distinct.deinit(allocator);
        var out = ValueList.empty;
        try out.append(allocator, .{ .bool = distinct.items.len == input.len });
        return out;
    }
    if (std.mem.eql(u8, call.name, "select")) {
        if (call.args.len != 1) return error.InvalidFunction;
        var out = ValueList.empty;
        for (input, 0..) |item, idx| {
            var projection = try evalExpressionCtx(allocator, call.args[0], item, env, idx);
            defer projection.deinit(allocator);
            if (projection.items.len == 0) continue;
            try out.appendSlice(allocator, projection.items);
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "where")) {
        if (call.args.len != 1) return error.InvalidFunction;
        var out = ValueList.empty;
        for (input, 0..) |item, idx| {
            var criteria = try evalExpressionCtx(allocator, call.args[0], item, env, idx);
            defer criteria.deinit(allocator);
            if (criteria.items.len == 0) continue;
            if (criteria.items.len != 1 or criteria.items[0] != .bool) return error.InvalidPredicate;
            if (criteria.items[0].bool) {
                try out.append(allocator, item);
            }
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "single")) {
        var out = ValueList.empty;
        if (input.len == 1) {
            try out.append(allocator, input[0]);
            return out;
        }
        if (input.len == 0) return out;
        return error.SingletonRequired;
    }
    if (std.mem.eql(u8, call.name, "first")) {
        var out = ValueList.empty;
        if (input.len == 0) return out;
        try out.append(allocator, input[0]);
        return out;
    }
    if (std.mem.eql(u8, call.name, "last")) {
        var out = ValueList.empty;
        if (input.len == 0) return out;
        try out.append(allocator, input[input.len - 1]);
        return out;
    }
    if (std.mem.eql(u8, call.name, "tail")) {
        var out = ValueList.empty;
        if (input.len <= 1) return out;
        try out.appendSlice(allocator, input[1..]);
        return out;
    }
    if (std.mem.eql(u8, call.name, "skip")) {
        const num = try parseIntegerArg(call);
        if (num <= 0) return sliceValues(allocator, input);
        const len_i64: i64 = @intCast(input.len);
        if (num >= len_i64) return ValueList.empty;
        const offset: usize = @intCast(num);
        return sliceValues(allocator, input[offset..]);
    }
    if (std.mem.eql(u8, call.name, "take")) {
        const num = try parseIntegerArg(call);
        if (num <= 0) return ValueList.empty;
        const len_i64: i64 = @intCast(input.len);
        if (num >= len_i64) return sliceValues(allocator, input);
        const count: usize = @intCast(num);
        return sliceValues(allocator, input[0..count]);
    }
    return error.InvalidFunction;
}

fn parseIntegerArg(call: ast.FunctionCall) EvalError!i64 {
    if (call.args.len != 1) return error.InvalidFunction;
    return integerLiteral(call.args[0]) orelse error.InvalidFunction;
}

fn integerLiteral(expr: ast.Expr) ?i64 {
    return switch (expr) {
        .Literal => |lit| switch (lit) {
            .Number => |text| std.fmt.parseInt(i64, text, 10) catch null,
            else => null,
        },
        else => null,
    };
}

fn sliceValues(allocator: std.mem.Allocator, values: []const std.json.Value) EvalError!ValueList {
    var out = ValueList.empty;
    if (values.len == 0) return out;
    try out.appendSlice(allocator, values);
    return out;
}

fn literalToJson(lit: ast.Literal) std.json.Value {
    return switch (lit) {
        .Null => .null,
        .Bool => |b| .{ .bool = b },
        .String => |s| .{ .string = s },
        .Number => |n| parseNumberLiteral(n),
        .Date => |d| .{ .string = d },
        .DateTime => |d| .{ .string = d },
        .Time => |t| .{ .string = t },
    };
}

fn parseNumberLiteral(text: []const u8) std.json.Value {
    if (std.mem.indexOfScalar(u8, text, '.') != null) {
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

fn distinctValues(
    allocator: std.mem.Allocator,
    input: []const std.json.Value,
) EvalError!ValueList {
    var out = ValueList.empty;
    for (input) |item| {
        var seen = false;
        for (out.items) |existing| {
            if (jsonEqual(item, existing)) {
                seen = true;
                break;
            }
        }
        if (!seen) try out.append(allocator, item);
    }
    return out;
}

fn jsonEqual(a: std.json.Value, b: std.json.Value) bool {
    if (!std.mem.eql(u8, @tagName(a), @tagName(b))) {
        const na = numberValue(a);
        const nb = numberValue(b);
        if (na != null and nb != null) {
            return na.? == nb.?;
        }
        return false;
    }
    switch (a) {
        .null => return true,
        .bool => |v| return v == b.bool,
        .integer => |v| return v == b.integer,
        .float => |v| return v == b.float,
        .number_string => |v| return std.mem.eql(u8, v, b.number_string),
        .string => |v| return std.mem.eql(u8, v, b.string),
        .array => |arr| {
            const arr_b = b.array;
            if (arr.items.len != arr_b.items.len) return false;
            for (arr.items, 0..) |item, idx| {
                if (!jsonEqual(item, arr_b.items[idx])) return false;
            }
            return true;
        },
        .object => |obj| {
            const obj_b = b.object;
            if (obj.count() != obj_b.count()) return false;
            var it = obj.iterator();
            while (it.next()) |entry| {
                const other = obj_b.get(entry.key_ptr.*) orelse return false;
                if (!jsonEqual(entry.value_ptr.*, other)) return false;
            }
            return true;
        },
    }
}

fn numberValue(v: std.json.Value) ?f64 {
    return switch (v) {
        .integer => |i| @floatFromInt(i),
        .float => |f| f,
        .number_string => |s| std.fmt.parseFloat(f64, s) catch null,
        else => null,
    };
}
