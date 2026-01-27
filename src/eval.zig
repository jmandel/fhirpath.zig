const std = @import("std");
const ast = @import("ast.zig");
const jsondoc = @import("jsondoc.zig");
const item = @import("item.zig");
const schema = @import("schema.zig");

pub const ItemList = std.ArrayList(item.Item);

pub const EvalContext = struct {
    allocator: std.mem.Allocator,
    doc: *jsondoc.JsonDoc,
    types: *item.TypeTable,
    schema: ?*schema.Schema,
};

pub const Env = struct {
    map: std.StringHashMap([]const item.Item),

    pub fn init(allocator: std.mem.Allocator) Env {
        return .{ .map = std.StringHashMap([]const item.Item).init(allocator) };
    }

    pub fn deinit(self: *Env) void {
        self.map.deinit();
    }

    pub fn put(self: *Env, name: []const u8, values: []const item.Item) !void {
        try self.map.put(name, values);
    }
};

const EvalError = error{ InvalidFunction, InvalidPredicate, SingletonRequired } || error{OutOfMemory, InvalidJson, TrailingData};

pub const EvalResult = struct {
    items: ItemList,
    doc: jsondoc.JsonDoc,

    pub fn deinit(self: *EvalResult) void {
        self.items.deinit();
        self.doc.deinit();
    }
};

pub fn evalWithJson(
    allocator: std.mem.Allocator,
    expr: ast.Expr,
    json_text: []const u8,
    env: ?*Env,
    types: *item.TypeTable,
    schema_ptr: ?*schema.Schema,
) !EvalResult {
    var doc = try jsondoc.JsonDoc.init(allocator, json_text);
    var ctx = EvalContext{ .allocator = allocator, .doc = &doc, .types = types, .schema = schema_ptr };
    const items = try evalExpression(&ctx, expr, doc.root, env);
    return .{ .items = items, .doc = doc };
}

pub fn evalExpression(
    ctx: *EvalContext,
    expr: ast.Expr,
    root: jsondoc.NodeIndex,
    env: ?*Env,
) EvalError!ItemList {
    const root_item = try itemFromNode(ctx, root);
    return evalExpressionCtx(ctx, expr, root_item, env, null);
}

fn evalExpressionCtx(
    ctx: *EvalContext,
    expr: ast.Expr,
    root_item: item.Item,
    env: ?*Env,
    index: ?usize,
) EvalError!ItemList {
    switch (expr) {
        .Path => |p| return evalPath(ctx, p, root_item, env, index),
        .Binary => |b| return evalBinary(ctx, b, root_item, env, index),
        .Literal => |lit| {
            var out = ItemList.empty;
            try out.append(ctx.allocator, literalToItem(ctx, lit));
            return out;
        },
    }
}

fn evalPath(
    ctx: *EvalContext,
    path: ast.PathExpr,
    root_item: item.Item,
    env: ?*Env,
    index: ?usize,
) EvalError!ItemList {
    var current = ItemList.empty;

    switch (path.root) {
        .This => try current.append(ctx.allocator, root_item),
        .Env => |name| {
            if (env) |e| {
                if (e.map.get(name)) |vals| {
                    try current.appendSlice(ctx.allocator, vals);
                }
            }
        },
        .Index => {
            if (index) |idx| {
                try current.append(ctx.allocator, makeIntegerItem(ctx, @intCast(idx)));
            }
        },
    }

    for (path.steps) |step| {
        switch (step) {
            .Property => |name| {
                var next = ItemList.empty;
                for (current.items) |it| {
                    try applySegment(ctx, it, name, &next);
                }
                current.deinit(ctx.allocator);
                current = next;
            },
            .Index => |idx| {
                var next = ItemList.empty;
                if (idx < current.items.len) {
                    try next.append(ctx.allocator, current.items[idx]);
                }
                current.deinit(ctx.allocator);
                current = next;
            },
            .Function => |call| {
                const next = try evalFunction(ctx, call, current.items, env);
                current.deinit(ctx.allocator);
                current = next;
            },
        }
    }

    return current;
}

fn applySegment(
    ctx: *EvalContext,
    it: item.Item,
    name: []const u8,
    out: *ItemList,
) !void {
    if (it.node == null) return;
    const node = ctx.doc.node(@intCast(it.node.?)).*;
    var child_type_id: u32 = 0;
    if (ctx.schema) |s| {
        if (schema.isModelType(it.type_id)) {
            if (s.childTypeForField(it.type_id, name)) |tid| child_type_id = tid;
        }
    }
    switch (node.kind) {
        .object => {
            for (node.data.object) |field| {
                if (std.mem.eql(u8, field.key, name)) {
                    const child_node = ctx.doc.node(field.value).*;
                    if (child_node.kind == .array) {
                        for (child_node.data.array) |child_idx| {
                            const child = try itemFromNodeWithType(ctx, child_idx, child_type_id);
                            try out.append(ctx.allocator, child);
                        }
                    } else {
                        const child = try itemFromNodeWithType(ctx, field.value, child_type_id);
                        try out.append(ctx.allocator, child);
                    }
                }
            }
        },
        .array => {
            for (node.data.array) |child_idx| {
                const child = try itemFromNodeWithType(ctx, child_idx, child_type_id);
                try applySegment(ctx, child, name, out);
            }
        },
        else => {},
    }
}

fn evalBinary(
    ctx: *EvalContext,
    expr: ast.BinaryExpr,
    root_item: item.Item,
    env: ?*Env,
    index: ?usize,
) EvalError!ItemList {
    _ = expr.op;
    var left = try evalExpressionCtx(ctx, expr.left.*, root_item, env, index);
    defer left.deinit(ctx.allocator);
    var right = try evalExpressionCtx(ctx, expr.right.*, root_item, env, index);
    defer right.deinit(ctx.allocator);
    return evalEquality(ctx, left.items, right.items);
}

fn evalEquality(
    ctx: *EvalContext,
    left: []const item.Item,
    right: []const item.Item,
) EvalError!ItemList {
    var out = ItemList.empty;
    if (left.len == 0 or right.len == 0) return out;
    var matched = false;
    for (left) |l| {
        for (right) |r| {
            if (itemsEqual(ctx, l, r)) {
                matched = true;
                break;
            }
        }
        if (matched) break;
    }
    try out.append(ctx.allocator, makeBoolItem(ctx, matched));
    return out;
}

fn evalFunction(
    ctx: *EvalContext,
    call: ast.FunctionCall,
    input: []const item.Item,
    env: ?*Env,
) EvalError!ItemList {
    if (std.mem.eql(u8, call.name, "count")) {
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(input.len)));
        return out;
    }
    if (std.mem.eql(u8, call.name, "empty")) {
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, input.len == 0));
        return out;
    }
    if (std.mem.eql(u8, call.name, "exists")) {
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, input.len != 0));
        return out;
    }
    if (std.mem.eql(u8, call.name, "distinct")) {
        return distinctItems(ctx, input);
    }
    if (std.mem.eql(u8, call.name, "isDistinct")) {
        var distinct = try distinctItems(ctx, input);
        defer distinct.deinit(ctx.allocator);
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, distinct.items.len == input.len));
        return out;
    }
    if (std.mem.eql(u8, call.name, "select")) {
        if (call.args.len != 1) return error.InvalidFunction;
        var out = ItemList.empty;
        for (input, 0..) |it, idx| {
            var projection = try evalExpressionCtx(ctx, call.args[0], it, env, idx);
            defer projection.deinit(ctx.allocator);
            if (projection.items.len == 0) continue;
            try out.appendSlice(ctx.allocator, projection.items);
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "where")) {
        if (call.args.len != 1) return error.InvalidFunction;
        var out = ItemList.empty;
        for (input, 0..) |it, idx| {
            var criteria = try evalExpressionCtx(ctx, call.args[0], it, env, idx);
            defer criteria.deinit(ctx.allocator);
            if (criteria.items.len == 0) continue;
            if (criteria.items.len != 1) return error.InvalidPredicate;
            const crit = criteria.items[0];
            if (!itemIsBool(ctx, crit)) return error.InvalidPredicate;
            if (itemBoolValue(ctx, crit)) {
                try out.append(ctx.allocator, it);
            }
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "single")) {
        var out = ItemList.empty;
        if (input.len == 1) {
            try out.append(ctx.allocator, input[0]);
            return out;
        }
        if (input.len == 0) return out;
        return error.SingletonRequired;
    }
    if (std.mem.eql(u8, call.name, "first")) {
        var out = ItemList.empty;
        if (input.len == 0) return out;
        try out.append(ctx.allocator, input[0]);
        return out;
    }
    if (std.mem.eql(u8, call.name, "last")) {
        var out = ItemList.empty;
        if (input.len == 0) return out;
        try out.append(ctx.allocator, input[input.len - 1]);
        return out;
    }
    if (std.mem.eql(u8, call.name, "tail")) {
        var out = ItemList.empty;
        if (input.len <= 1) return out;
        try out.appendSlice(ctx.allocator, input[1..]);
        return out;
    }
    if (std.mem.eql(u8, call.name, "skip")) {
        const num = try parseIntegerArg(call);
        if (num <= 0) return sliceItems(ctx, input);
        const len_i64: i64 = @intCast(input.len);
        if (num >= len_i64) return ItemList.empty;
        const offset: usize = @intCast(num);
        return sliceItems(ctx, input[offset..]);
    }
    if (std.mem.eql(u8, call.name, "take")) {
        const num = try parseIntegerArg(call);
        if (num <= 0) return ItemList.empty;
        const len_i64: i64 = @intCast(input.len);
        if (num >= len_i64) return sliceItems(ctx, input);
        const count: usize = @intCast(num);
        return sliceItems(ctx, input[0..count]);
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

fn sliceItems(ctx: *EvalContext, values: []const item.Item) EvalError!ItemList {
    var out = ItemList.empty;
    if (values.len == 0) return out;
    try out.appendSlice(ctx.allocator, values);
    return out;
}

fn distinctItems(ctx: *EvalContext, input: []const item.Item) EvalError!ItemList {
    var out = ItemList.empty;
    for (input) |it| {
        var seen = false;
        for (out.items) |existing| {
            if (itemsEqual(ctx, it, existing)) {
                seen = true;
                break;
            }
        }
        if (!seen) try out.append(ctx.allocator, it);
    }
    return out;
}

fn itemsEqual(ctx: *EvalContext, a: item.Item, b: item.Item) bool {
    if (a.data_kind == .value and b.data_kind == .value) {
        return valueEqual(a.value.?, b.value.?);
    }
    if (a.data_kind == .json_span and b.data_kind == .json_span and a.node != null and b.node != null) {
        return nodeEqual(ctx.doc, @intCast(a.node.?), @intCast(b.node.?));
    }
    return valueEqual(itemToValue(ctx, a), itemToValue(ctx, b));
}

fn valueEqual(a: item.Value, b: item.Value) bool {
    if (!std.mem.eql(u8, @tagName(a), @tagName(b))) {
        const na = valueToNumber(a);
        const nb = valueToNumber(b);
        if (na != null and nb != null) return na.? == nb.?;
        return false;
    }
    switch (a) {
        .empty => return true,
        .boolean => |v| return v == b.boolean,
        .integer => |v| return v == b.integer,
        .decimal => |v| return std.mem.eql(u8, v, b.decimal),
        .string => |v| return std.mem.eql(u8, v, b.string),
        .date => |v| return std.mem.eql(u8, v, b.date),
        .time => |v| return std.mem.eql(u8, v, b.time),
        .dateTime => |v| return std.mem.eql(u8, v, b.dateTime),
        .quantity => |v| return std.mem.eql(u8, v.value, b.quantity.value) and std.mem.eql(u8, v.unit, b.quantity.unit),
    }
}

fn valueToNumber(v: item.Value) ?f64 {
    return switch (v) {
        .integer => |i| @floatFromInt(i),
        .decimal => |s| std.fmt.parseFloat(f64, s) catch null,
        else => null,
    };
}

fn nodeEqual(doc: *jsondoc.JsonDoc, a_idx: jsondoc.NodeIndex, b_idx: jsondoc.NodeIndex) bool {
    if (a_idx == b_idx) return true;
    const a = doc.node(a_idx).*;
    const b = doc.node(b_idx).*;
    if (a.kind != b.kind) {
        if (a.kind == .number and b.kind == .number) {
            return numberEqual(a.data.number, b.data.number);
        }
        return false;
    }
    switch (a.kind) {
        .null => return true,
        .bool => return a.data.bool == b.data.bool,
        .number => return numberEqual(a.data.number, b.data.number),
        .string => return std.mem.eql(u8, a.data.string, b.data.string),
        .array => {
            const arr_a = a.data.array;
            const arr_b = b.data.array;
            if (arr_a.len != arr_b.len) return false;
            for (arr_a, 0..) |child, idx| {
                if (!nodeEqual(doc, child, arr_b[idx])) return false;
            }
            return true;
        },
        .object => {
            const obj_a = a.data.object;
            const obj_b = b.data.object;
            if (obj_a.len != obj_b.len) return false;
            for (obj_a) |field| {
                var found = false;
                for (obj_b) |other| {
                    if (std.mem.eql(u8, field.key, other.key)) {
                        if (!nodeEqual(doc, field.value, other.value)) return false;
                        found = true;
                        break;
                    }
                }
                if (!found) return false;
            }
            return true;
        },
    }
}

fn numberEqual(a: []const u8, b: []const u8) bool {
    const na = std.fmt.parseFloat(f64, a) catch return std.mem.eql(u8, a, b);
    const nb = std.fmt.parseFloat(f64, b) catch return std.mem.eql(u8, a, b);
    return na == nb;
}

fn literalToItem(ctx: *EvalContext, lit: ast.Literal) item.Item {
    return switch (lit) {
        .Null => makeEmptyItem(ctx),
        .Bool => |b| makeBoolItem(ctx, b),
        .String => |s| makeStringItem(ctx, s),
        .Number => |n| makeNumberItem(ctx, n),
        .Date => |d| makeDateItem(ctx, d),
        .DateTime => |d| makeDateTimeItem(ctx, d),
        .Time => |t| makeTimeItem(ctx, t),
    };
}

fn makeNodeItem(ctx: *EvalContext, node_idx: jsondoc.NodeIndex, type_id_override: u32) !item.Item {
    const node = ctx.doc.node(node_idx).*;
    const type_id = if (type_id_override != 0) type_id_override else try typeIdForNode(ctx, node_idx);
    return .{
        .data_kind = .json_span,
        .value_kind = .empty,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .data_pos = node.start,
        .data_end = node.end,
        .node = node_idx,
        .value = null,
    };
}

fn itemFromNode(ctx: *EvalContext, node_idx: jsondoc.NodeIndex) !item.Item {
    return makeNodeItem(ctx, node_idx, 0);
}

fn itemFromNodeWithType(ctx: *EvalContext, node_idx: jsondoc.NodeIndex, type_id: u32) !item.Item {
    return makeNodeItem(ctx, node_idx, type_id);
}

fn makeEmptyItem(ctx: *EvalContext) item.Item {
    _ = ctx;
    return .{
        .data_kind = .value,
        .value_kind = .empty,
        .type_id = 0,
        .source_pos = 0,
        .source_end = 0,
        .data_pos = 0,
        .data_end = 0,
        .node = null,
        .value = .{ .empty = {} },
    };
}

fn makeBoolItem(ctx: *EvalContext, v: bool) item.Item {
    const type_id = ctx.types.getOrAdd("System.Boolean") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .boolean,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .data_pos = 0,
        .data_end = 0,
        .node = null,
        .value = .{ .boolean = v },
    };
}

fn makeIntegerItem(ctx: *EvalContext, v: i64) item.Item {
    const type_id = ctx.types.getOrAdd("System.Integer") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .integer,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .data_pos = 0,
        .data_end = 0,
        .node = null,
        .value = .{ .integer = v },
    };
}

fn makeNumberItem(ctx: *EvalContext, raw: []const u8) item.Item {
    if (isInteger(raw)) {
        const parsed = std.fmt.parseInt(i64, raw, 10) catch 0;
        return makeIntegerItem(ctx, parsed);
    }
    const type_id = ctx.types.getOrAdd("System.Decimal") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .decimal,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .data_pos = 0,
        .data_end = 0,
        .node = null,
        .value = .{ .decimal = raw },
    };
}

fn makeStringItem(ctx: *EvalContext, s: []const u8) item.Item {
    const type_id = ctx.types.getOrAdd("System.String") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .string,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .data_pos = 0,
        .data_end = 0,
        .node = null,
        .value = .{ .string = s },
    };
}

fn makeDateItem(ctx: *EvalContext, s: []const u8) item.Item {
    const type_id = ctx.types.getOrAdd("System.Date") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .date,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .data_pos = 0,
        .data_end = 0,
        .node = null,
        .value = .{ .date = s },
    };
}

fn makeDateTimeItem(ctx: *EvalContext, s: []const u8) item.Item {
    const type_id = ctx.types.getOrAdd("System.DateTime") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .dateTime,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .data_pos = 0,
        .data_end = 0,
        .node = null,
        .value = .{ .dateTime = s },
    };
}

fn makeTimeItem(ctx: *EvalContext, s: []const u8) item.Item {
    const type_id = ctx.types.getOrAdd("System.Time") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .time,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .data_pos = 0,
        .data_end = 0,
        .node = null,
        .value = .{ .time = s },
    };
}

fn itemIsBool(ctx: *EvalContext, it: item.Item) bool {
    if (it.data_kind == .value and it.value != null and it.value.? == .boolean) return true;
    if (it.data_kind == .json_span and it.node != null) {
        const node = ctx.doc.node(@intCast(it.node.?)).*;
        return node.kind == .bool;
    }
    return false;
}

fn itemBoolValue(ctx: *EvalContext, it: item.Item) bool {
    if (it.data_kind == .value and it.value != null and it.value.? == .boolean) return it.value.?.boolean;
    if (it.data_kind == .json_span and it.node != null) {
        const node = ctx.doc.node(@intCast(it.node.?)).*;
        return node.data.bool;
    }
    return false;
}

fn itemToValue(ctx: *EvalContext, it: item.Item) item.Value {
    if (it.data_kind == .value and it.value != null) return it.value.?;
    if (it.data_kind == .json_span and it.node != null) {
        const node = ctx.doc.node(@intCast(it.node.?)).*;
        return switch (node.kind) {
            .null => .{ .empty = {} },
            .bool => .{ .boolean = node.data.bool },
            .number => .{ .decimal = node.data.number },
            .string => .{ .string = node.data.string },
            else => .{ .empty = {} },
        };
    }
    return .{ .empty = {} };
}

fn typeIdForNode(ctx: *EvalContext, node_idx: jsondoc.NodeIndex) !u32 {
    const node = ctx.doc.node(node_idx).*;
    return switch (node.kind) {
        .null => ctx.types.getOrAdd("System.Any"),
        .bool => ctx.types.getOrAdd("System.Boolean"),
        .number => if (isInteger(node.data.number)) ctx.types.getOrAdd("System.Integer") else ctx.types.getOrAdd("System.Decimal"),
        .string => {
            if (isDateTime(node.data.string)) return ctx.types.getOrAdd("System.DateTime");
            if (isDate(node.data.string)) return ctx.types.getOrAdd("System.Date");
            if (isTime(node.data.string)) return ctx.types.getOrAdd("System.Time");
            return ctx.types.getOrAdd("System.String");
        },
        .object => {
            if (resourceTypeName(ctx, node)) |rt| {
                if (ctx.schema) |s| {
                    if (s.typeIdByLocalName(rt)) |tid| return tid;
                }
            }
            return ctx.types.getOrAdd("System.Any");
        },
        .array => ctx.types.getOrAdd("System.Any"),
    };
}

fn resourceTypeName(ctx: *EvalContext, node: jsondoc.Node) ?[]const u8 {
    if (node.kind != .object) return null;
    for (node.data.object) |field| {
        if (!std.mem.eql(u8, field.key, "resourceType")) continue;
        const value_node = ctx.doc.node(field.value).*;
        if (value_node.kind == .string) return value_node.data.string;
    }
    return null;
}

fn isInteger(raw: []const u8) bool {
    return std.mem.indexOfScalar(u8, raw, '.') == null and std.mem.indexOfAny(u8, raw, "eE") == null;
}

fn isDateTime(s: []const u8) bool {
    return std.mem.indexOfScalar(u8, s, 'T') != null;
}

fn isDate(s: []const u8) bool {
    if (std.mem.indexOfScalar(u8, s, 'T') != null) return false;
    return std.mem.indexOfScalar(u8, s, '-') != null;
}

fn isTime(s: []const u8) bool {
    return std.mem.indexOfScalar(u8, s, ':') != null and std.mem.indexOfScalar(u8, s, 'T') == null;
}
