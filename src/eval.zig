const std = @import("std");
const ast = @import("ast.zig");
const jsondoc = @import("jsondoc.zig");
const node = @import("node.zig");
const item = @import("item.zig");
const schema = @import("schema.zig");
const JsonDocAdapter = @import("backends/jsondoc.zig").JsonDocAdapter;

pub const ItemList = std.ArrayList(item.Item);

pub fn EvalContext(comptime A: type) type {
    return struct {
        allocator: std.mem.Allocator,
        adapter: *A,
        types: *item.TypeTable,
        schema: ?*schema.Schema,
        root_item: item.Item = undefined,
    };
}

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

const EvalError = error{ InvalidFunction, InvalidPredicate, SingletonRequired, InvalidOperand } || error{OutOfMemory, InvalidJson, TrailingData};

pub const EvalResult = struct {
    items: ItemList,
    doc: jsondoc.JsonDoc,

    pub fn deinit(self: *EvalResult) void {
        self.items.deinit(self.doc.allocator);
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
    var adapter = JsonDocAdapter.init(&doc);
    var ctx = EvalContext(JsonDocAdapter){ .allocator = allocator, .adapter = &adapter, .types = types, .schema = schema_ptr };
    const items = try evalExpression(&ctx, expr, adapter.root(), env);
    return .{ .items = items, .doc = doc };
}

pub fn evalExpression(
    ctx: anytype,
    expr: ast.Expr,
    root: @TypeOf(ctx.adapter.*).NodeRef,
    env: ?*Env,
) EvalError!ItemList {
    const root_item = try itemFromNode(ctx, root);
    ctx.root_item = root_item;
    return evalExpressionCtx(ctx, expr, root_item, env, null);
}

fn rawFromNodeRef(comptime A: type, ref: A.NodeRef) usize {
    return switch (@typeInfo(A.NodeRef)) {
        .pointer => @intFromPtr(ref),
        .int, .comptime_int => @intCast(ref),
        else => @compileError("NodeRef must be pointer or integer"),
    };
}

fn nodeRefFromRaw(comptime A: type, raw: usize) A.NodeRef {
    return switch (@typeInfo(A.NodeRef)) {
        .pointer => @ptrFromInt(raw),
        .int, .comptime_int => @intCast(raw),
        else => @compileError("NodeRef must be pointer or integer"),
    };
}

fn evalExpressionCtx(
    ctx: anytype,
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
        .TypeExpr => |t| return evalTypeExpr(ctx, t, root_item, env, index),
        .Unary => |u| return evalUnary(ctx, u, root_item, env, index),
        .Invoke => |inv| return evalInvoke(ctx, inv, root_item, env, index),
    }
}

fn evalPath(
    ctx: anytype,
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
        .Total => {
            // $total is used in aggregate() - needs to be passed through env
            if (env) |e| {
                if (e.map.get("$total")) |vals| {
                    try current.appendSlice(ctx.allocator, vals);
                }
            }
        },
        .Literal => |lit| {
            try current.append(ctx.allocator, literalToItem(ctx, lit));
        },
        .Empty => {
            // Empty collection - return nothing
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
    ctx: anytype,
    it: item.Item,
    name: []const u8,
    out: *ItemList,
) !void {
    if (it.node == null) return;
    const A = @TypeOf(ctx.adapter.*);
    const ref = nodeRefFromRaw(A, it.node.?);
    var child_type_id: u32 = 0;
    if (ctx.schema) |s| {
        if (schema.isModelType(it.type_id)) {
            if (s.childTypeForField(it.type_id, name)) |tid| child_type_id = tid;
        }
    }
    switch (A.kind(ctx.adapter, ref)) {
        .object => {
            if (A.objectGet(ctx.adapter, ref, name)) |child_ref| {
                if (A.kind(ctx.adapter, child_ref) == .array) {
                    const len = A.arrayLen(ctx.adapter, child_ref);
                    for (0..len) |i| {
                        const child_item_ref = A.arrayAt(ctx.adapter, child_ref, i);
                        const child = try itemFromNodeWithType(ctx, child_item_ref, child_type_id);
                        try out.append(ctx.allocator, child);
                    }
                } else {
                    const child = try itemFromNodeWithType(ctx, child_ref, child_type_id);
                    try out.append(ctx.allocator, child);
                }
            }
        },
        .array => {
            const len = A.arrayLen(ctx.adapter, ref);
            for (0..len) |i| {
                const child_ref = A.arrayAt(ctx.adapter, ref, i);
                const child = try itemFromNodeWithType(ctx, child_ref, child_type_id);
                try applySegment(ctx, child, name, out);
            }
        },
        else => {},
    }
}

fn evalBinary(
    ctx: anytype,
    expr: ast.BinaryExpr,
    root_item: item.Item,
    env: ?*Env,
    index: ?usize,
) EvalError!ItemList {
    // Handle short-circuit operators specially
    switch (expr.op) {
        .And => return evalAnd(ctx, expr, root_item, env, index),
        .Or => return evalOr(ctx, expr, root_item, env, index),
        .Implies => return evalImplies(ctx, expr, root_item, env, index),
        else => {},
    }

    // For other operators, evaluate both sides first
    var left = try evalExpressionCtx(ctx, expr.left.*, root_item, env, index);
    defer left.deinit(ctx.allocator);
    var right = try evalExpressionCtx(ctx, expr.right.*, root_item, env, index);
    defer right.deinit(ctx.allocator);

    return switch (expr.op) {
        // Equality
        .Eq => evalEquality(ctx, left.items, right.items),
        .NotEq => evalNotEquality(ctx, left.items, right.items),
        .Equiv => evalEquivalence(ctx, left.items, right.items),
        .NotEquiv => evalNotEquivalence(ctx, left.items, right.items),

        // Comparison
        .Lt => evalComparison(ctx, left.items, right.items, .lt),
        .LtEq => evalComparison(ctx, left.items, right.items, .le),
        .Gt => evalComparison(ctx, left.items, right.items, .gt),
        .GtEq => evalComparison(ctx, left.items, right.items, .ge),

        // Boolean (xor not short-circuit)
        .Xor => evalXor(ctx, left.items, right.items),

        // Union
        .Union => evalUnionOp(ctx, left.items, right.items),

        // Arithmetic
        .Add => evalArithmetic(ctx, left.items, right.items, .add),
        .Sub => evalArithmetic(ctx, left.items, right.items, .sub),
        .Mul => evalArithmetic(ctx, left.items, right.items, .mul),
        .Div => evalArithmetic(ctx, left.items, right.items, .div),
        .IntDiv => evalArithmetic(ctx, left.items, right.items, .int_div),
        .Mod => evalArithmetic(ctx, left.items, right.items, .mod),

        // String concatenation
        .Concat => evalConcat(ctx, left.items, right.items),

        // Membership
        .In => evalIn(ctx, left.items, right.items),
        .Contains => evalIn(ctx, right.items, left.items), // contains is reverse of in

        // Short-circuit operators already handled above
        .And, .Or, .Implies => unreachable,
    };
}

fn evalEquality(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
) EvalError!ItemList {
    var out = ItemList.empty;
    if (left.len == 0 or right.len == 0) return out;
    // For singleton comparison
    if (left.len == 1 and right.len == 1) {
        const eq = itemsEqual(ctx, left[0], right[0]);
        try out.append(ctx.allocator, makeBoolItem(ctx, eq));
        return out;
    }
    // For collections: are they the same?
    if (left.len != right.len) {
        try out.append(ctx.allocator, makeBoolItem(ctx, false));
        return out;
    }
    // Check each item
    for (left, 0..) |l, i| {
        if (!itemsEqual(ctx, l, right[i])) {
            try out.append(ctx.allocator, makeBoolItem(ctx, false));
            return out;
        }
    }
    try out.append(ctx.allocator, makeBoolItem(ctx, true));
    return out;
}

fn evalNotEquality(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
) EvalError!ItemList {
    var out = ItemList.empty;
    if (left.len == 0 or right.len == 0) return out;
    var eq_result = try evalEquality(ctx, left, right);
    defer eq_result.deinit(ctx.allocator);
    if (eq_result.items.len == 1) {
        const eq = itemBoolValue(ctx, eq_result.items[0]);
        try out.append(ctx.allocator, makeBoolItem(ctx, !eq));
    }
    return out;
}

fn evalEquivalence(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
) EvalError!ItemList {
    var out = ItemList.empty;
    // Equivalence always returns true or false (never empty)
    // Empty collections are equivalent to each other
    if (left.len == 0 and right.len == 0) {
        try out.append(ctx.allocator, makeBoolItem(ctx, true));
        return out;
    }
    if (left.len == 0 or right.len == 0) {
        try out.append(ctx.allocator, makeBoolItem(ctx, false));
        return out;
    }
    if (left.len != right.len) {
        try out.append(ctx.allocator, makeBoolItem(ctx, false));
        return out;
    }
    for (left, 0..) |l, i| {
        if (!itemsEquivalent(ctx, l, right[i])) {
            try out.append(ctx.allocator, makeBoolItem(ctx, false));
            return out;
        }
    }
    try out.append(ctx.allocator, makeBoolItem(ctx, true));
    return out;
}

fn evalNotEquivalence(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
) EvalError!ItemList {
    var out = ItemList.empty;
    var equiv_result = try evalEquivalence(ctx, left, right);
    defer equiv_result.deinit(ctx.allocator);
    if (equiv_result.items.len == 1) {
        const eq = itemBoolValue(ctx, equiv_result.items[0]);
        try out.append(ctx.allocator, makeBoolItem(ctx, !eq));
    }
    return out;
}

fn itemsEquivalent(ctx: anytype, a: item.Item, b: item.Item) bool {
    // For now, use same logic as equality
    // TODO: Implement proper equivalence (case-insensitive strings, etc.)
    return itemsEqual(ctx, a, b);
}

const CompareOp = enum { lt, le, gt, ge };

fn evalComparison(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
    op: CompareOp,
) EvalError!ItemList {
    var out = ItemList.empty;
    if (left.len != 1 or right.len != 1) return out; // Empty if not singletons

    const l = left[0];
    const r = right[0];

    const cmp = compareItems(ctx, l, r) orelse return out;
    const result = switch (op) {
        .lt => cmp < 0,
        .le => cmp <= 0,
        .gt => cmp > 0,
        .ge => cmp >= 0,
    };
    try out.append(ctx.allocator, makeBoolItem(ctx, result));
    return out;
}

fn compareItems(ctx: anytype, a: item.Item, b: item.Item) ?i32 {
    const va = itemToValue(ctx, a);
    const vb = itemToValue(ctx, b);

    // Compare integers
    if (va == .integer and vb == .integer) {
        if (va.integer < vb.integer) return -1;
        if (va.integer > vb.integer) return 1;
        return 0;
    }

    // Compare decimals or mixed int/decimal
    const fa: ?f64 = switch (va) {
        .integer => |i| @floatFromInt(i),
        .decimal => |d| std.fmt.parseFloat(f64, d) catch null,
        else => null,
    };
    const fb: ?f64 = switch (vb) {
        .integer => |i| @floatFromInt(i),
        .decimal => |d| std.fmt.parseFloat(f64, d) catch null,
        else => null,
    };
    if (fa != null and fb != null) {
        if (fa.? < fb.?) return -1;
        if (fa.? > fb.?) return 1;
        return 0;
    }

    // Compare strings
    if (va == .string and vb == .string) {
        return switch (std.mem.order(u8, va.string, vb.string)) {
            .lt => -1,
            .eq => 0,
            .gt => 1,
        };
    }

    return null;
}

// Boolean operators with three-valued logic
fn evalAnd(
    ctx: anytype,
    expr: ast.BinaryExpr,
    root_item: item.Item,
    env: ?*Env,
    index: ?usize,
) EvalError!ItemList {
    var out = ItemList.empty;

    // Evaluate left side
    var left = try evalExpressionCtx(ctx, expr.left.*, root_item, env, index);
    defer left.deinit(ctx.allocator);

    // Three-valued AND logic:
    // false AND anything = false
    // true AND true = true
    // true AND false = false
    // true AND {} = {}
    // {} AND true = {}
    // {} AND {} = {}
    // {} AND false = false

    const left_bool = toBoolTernary(ctx, left.items);

    if (left_bool == .false_val) {
        // Short-circuit: false AND anything = false
        try out.append(ctx.allocator, makeBoolItem(ctx, false));
        return out;
    }

    // Evaluate right side
    var right = try evalExpressionCtx(ctx, expr.right.*, root_item, env, index);
    defer right.deinit(ctx.allocator);

    const right_bool = toBoolTernary(ctx, right.items);

    if (right_bool == .false_val) {
        // anything AND false = false
        try out.append(ctx.allocator, makeBoolItem(ctx, false));
        return out;
    }

    if (left_bool == .true_val and right_bool == .true_val) {
        try out.append(ctx.allocator, makeBoolItem(ctx, true));
        return out;
    }

    // Otherwise empty (true AND {} or {} AND true or {} AND {})
    return out;
}

fn evalOr(
    ctx: anytype,
    expr: ast.BinaryExpr,
    root_item: item.Item,
    env: ?*Env,
    index: ?usize,
) EvalError!ItemList {
    var out = ItemList.empty;

    var left = try evalExpressionCtx(ctx, expr.left.*, root_item, env, index);
    defer left.deinit(ctx.allocator);

    const left_bool = toBoolTernary(ctx, left.items);

    if (left_bool == .true_val) {
        // Short-circuit: true OR anything = true
        try out.append(ctx.allocator, makeBoolItem(ctx, true));
        return out;
    }

    var right = try evalExpressionCtx(ctx, expr.right.*, root_item, env, index);
    defer right.deinit(ctx.allocator);

    const right_bool = toBoolTernary(ctx, right.items);

    if (right_bool == .true_val) {
        try out.append(ctx.allocator, makeBoolItem(ctx, true));
        return out;
    }

    if (left_bool == .false_val and right_bool == .false_val) {
        try out.append(ctx.allocator, makeBoolItem(ctx, false));
        return out;
    }

    // Otherwise empty
    return out;
}

fn evalImplies(
    ctx: anytype,
    expr: ast.BinaryExpr,
    root_item: item.Item,
    env: ?*Env,
    index: ?usize,
) EvalError!ItemList {
    var out = ItemList.empty;

    var left = try evalExpressionCtx(ctx, expr.left.*, root_item, env, index);
    defer left.deinit(ctx.allocator);

    const left_bool = toBoolTernary(ctx, left.items);

    if (left_bool == .false_val) {
        // false implies anything = true
        try out.append(ctx.allocator, makeBoolItem(ctx, true));
        return out;
    }

    var right = try evalExpressionCtx(ctx, expr.right.*, root_item, env, index);
    defer right.deinit(ctx.allocator);

    const right_bool = toBoolTernary(ctx, right.items);

    if (right_bool == .true_val) {
        // anything implies true = true
        try out.append(ctx.allocator, makeBoolItem(ctx, true));
        return out;
    }

    if (left_bool == .true_val and right_bool == .false_val) {
        try out.append(ctx.allocator, makeBoolItem(ctx, false));
        return out;
    }

    // Otherwise empty
    return out;
}

fn evalXor(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
) EvalError!ItemList {
    var out = ItemList.empty;

    const left_bool = toBoolTernary(ctx, left);
    const right_bool = toBoolTernary(ctx, right);

    if (left_bool == .empty or right_bool == .empty) {
        return out;
    }

    const result = (left_bool == .true_val) != (right_bool == .true_val);
    try out.append(ctx.allocator, makeBoolItem(ctx, result));
    return out;
}

const TernaryBool = enum { true_val, false_val, empty };

fn toBoolTernary(ctx: anytype, items: []const item.Item) TernaryBool {
    if (items.len == 0) return .empty;
    if (items.len != 1) return .empty; // Non-singleton is treated as empty for boolean
    // Per FHIRPath spec: singleton non-boolean is truthy
    if (!itemIsBool(ctx, items[0])) return .true_val;
    return if (itemBoolValue(ctx, items[0])) .true_val else .false_val;
}

fn evalUnionOp(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
) EvalError!ItemList {
    // Union combines both collections with distinct
    var combined = ItemList.empty;
    try combined.appendSlice(ctx.allocator, left);
    try combined.appendSlice(ctx.allocator, right);
    defer combined.deinit(ctx.allocator);
    return distinctItems(ctx, combined.items);
}

const ArithOp = enum { add, sub, mul, div, int_div, mod };

fn evalArithmetic(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
    op: ArithOp,
) EvalError!ItemList {
    var out = ItemList.empty;
    if (left.len != 1 or right.len != 1) return out;

    const va = itemToValue(ctx, left[0]);
    const vb = itemToValue(ctx, right[0]);

    // Integer arithmetic
    if (va == .integer and vb == .integer) {
        const a = va.integer;
        const b = vb.integer;
        const result: ?i64 = switch (op) {
            .add => a +% b,
            .sub => a -% b,
            .mul => a *% b,
            .int_div => if (b != 0) @divTrunc(a, b) else null,
            .mod => if (b != 0) @mod(a, b) else null,
            .div => null, // Integer division with / returns decimal
        };
        if (result) |r| {
            try out.append(ctx.allocator, makeIntegerItem(ctx, r));
            return out;
        }
        if (op == .div and b != 0) {
            const fa: f64 = @floatFromInt(a);
            const fb: f64 = @floatFromInt(b);
            try out.append(ctx.allocator, try makeDecimalItem(ctx, fa / fb));
            return out;
        }
    }

    // Decimal arithmetic
    const fa: ?f64 = switch (va) {
        .integer => |i| @floatFromInt(i),
        .decimal => |d| std.fmt.parseFloat(f64, d) catch null,
        else => null,
    };
    const fb: ?f64 = switch (vb) {
        .integer => |i| @floatFromInt(i),
        .decimal => |d| std.fmt.parseFloat(f64, d) catch null,
        else => null,
    };

    if (fa != null and fb != null) {
        const a = fa.?;
        const b = fb.?;
        const result: ?f64 = switch (op) {
            .add => a + b,
            .sub => a - b,
            .mul => a * b,
            .div => if (b != 0) a / b else null,
            .int_div => if (b != 0) @trunc(a / b) else null,
            .mod => if (b != 0) @mod(a, b) else null,
        };
        if (result) |r| {
            try out.append(ctx.allocator, try makeDecimalItem(ctx, r));
        }
    }

    return out;
}

fn evalConcat(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
) EvalError!ItemList {
    var out = ItemList.empty;

    // Get string values, treating empty as empty string
    const left_str = if (left.len == 0) "" else blk: {
        const v = itemToValue(ctx, left[0]);
        break :blk if (v == .string) v.string else return out;
    };
    const right_str = if (right.len == 0) "" else blk: {
        const v = itemToValue(ctx, right[0]);
        break :blk if (v == .string) v.string else return out;
    };

    const result = try std.fmt.allocPrint(ctx.allocator, "{s}{s}", .{ left_str, right_str });
    try out.append(ctx.allocator, makeStringItem(ctx, result));
    return out;
}

fn evalIn(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
) EvalError!ItemList {
    var out = ItemList.empty;
    if (left.len == 0) return out;
    if (left.len != 1) return error.SingletonRequired;

    for (right) |r| {
        if (itemsEqual(ctx, left[0], r)) {
            try out.append(ctx.allocator, makeBoolItem(ctx, true));
            return out;
        }
    }
    try out.append(ctx.allocator, makeBoolItem(ctx, false));
    return out;
}

fn evalTypeExpr(
    ctx: anytype,
    expr: ast.TypeExprNode,
    root_item: item.Item,
    env: ?*Env,
    index: ?usize,
) EvalError!ItemList {
    var operand = try evalExpressionCtx(ctx, expr.operand.*, root_item, env, index);
    defer operand.deinit(ctx.allocator);

    var out = ItemList.empty;

    switch (expr.op) {
        .Is => {
            // is returns true if the operand is of the specified type
            if (operand.items.len == 0) {
                try out.append(ctx.allocator, makeBoolItem(ctx, false));
                return out;
            }
            // For singleton, check if it matches the type
            const matches = itemIsType(ctx, operand.items[0], expr.type_name);
            try out.append(ctx.allocator, makeBoolItem(ctx, matches));
        },
        .As => {
            // as returns the operand if it's of the specified type, empty otherwise
            if (operand.items.len == 0) return out;
            if (itemIsType(ctx, operand.items[0], expr.type_name)) {
                try out.append(ctx.allocator, operand.items[0]);
            }
        },
    }
    return out;
}

fn itemIsType(ctx: anytype, it: item.Item, type_name: []const u8) bool {
    // Normalize type name (remove System. prefix if present)
    const normalized = if (std.mem.startsWith(u8, type_name, "System."))
        type_name[7..]
    else
        type_name;

    // Check value kind for value items
    if (it.data_kind == .value and it.value != null) {
        return switch (it.value.?) {
            .boolean => std.mem.eql(u8, normalized, "Boolean"),
            .integer => std.mem.eql(u8, normalized, "Integer"),
            .decimal => std.mem.eql(u8, normalized, "Decimal"),
            .string => std.mem.eql(u8, normalized, "String"),
            .date => std.mem.eql(u8, normalized, "Date"),
            .time => std.mem.eql(u8, normalized, "Time"),
            .dateTime => std.mem.eql(u8, normalized, "DateTime"),
            .quantity => std.mem.eql(u8, normalized, "Quantity"),
            .empty => false,
        };
    }

    // For node-backed values, check the underlying JSON type
    if (it.data_kind == .node_ref and it.node != null) {
        const A = @TypeOf(ctx.adapter.*);
        const ref = nodeRefFromRaw(A, it.node.?);
        return switch (A.kind(ctx.adapter, ref)) {
            .bool => std.mem.eql(u8, normalized, "Boolean"),
            .number => std.mem.eql(u8, normalized, "Integer") or std.mem.eql(u8, normalized, "Decimal"),
            .string => std.mem.eql(u8, normalized, "String"),
            else => false,
        };
    }

    return false;
}

fn evalUnary(
    ctx: anytype,
    expr: ast.UnaryExpr,
    root_item: item.Item,
    env: ?*Env,
    index: ?usize,
) EvalError!ItemList {
    var operand = try evalExpressionCtx(ctx, expr.operand.*, root_item, env, index);
    defer operand.deinit(ctx.allocator);

    var out = ItemList.empty;
    if (operand.items.len == 0) return out;

    const it = operand.items[0];
    switch (expr.op) {
        .Plus => {
            // Unary + is identity for numbers
            try out.append(ctx.allocator, it);
        },
        .Minus => {
            // Unary - negates the number
            if (it.data_kind == .value and it.value != null) {
                switch (it.value.?) {
                    .integer => |val| {
                        try out.append(ctx.allocator, makeIntegerItem(ctx, -val));
                    },
                    .decimal => |val| {
                        const f = std.fmt.parseFloat(f64, val) catch return error.InvalidOperand;
                        try out.append(ctx.allocator, try makeDecimalItem(ctx, -f));
                    },
                    else => return error.InvalidOperand,
                }
            } else if (itemIsInteger(ctx, it)) {
                const val = itemIntegerValue(ctx, it) orelse return error.InvalidOperand;
                try out.append(ctx.allocator, makeIntegerItem(ctx, -val));
            } else {
                // Try to get as number from JSON
                const val = itemToValue(ctx, it);
                switch (val) {
                    .integer => |v| try out.append(ctx.allocator, makeIntegerItem(ctx, -v)),
                    .decimal => |v| {
                        const f = std.fmt.parseFloat(f64, v) catch return error.InvalidOperand;
                        try out.append(ctx.allocator, try makeDecimalItem(ctx, -f));
                    },
                    else => return error.InvalidOperand,
                }
            }
        },
    }
    return out;
}

fn evalInvoke(
    ctx: anytype,
    inv: ast.InvokeExpr,
    root_item: item.Item,
    env: ?*Env,
    index: ?usize,
) EvalError!ItemList {
    // First evaluate the operand
    var current = try evalExpressionCtx(ctx, inv.operand.*, root_item, env, index);

    // Then apply each step
    for (inv.steps) |step| {
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
                const result = try evalFunction(ctx, call, current.items, env);
                current.deinit(ctx.allocator);
                current = result;
            },
        }
    }

    return current;
}

fn evalFunction(
    ctx: anytype,
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
    if (std.mem.eql(u8, call.name, "not")) {
        // not() returns the boolean negation of the input
        // Per FHIRPath spec: non-boolean singleton is truthy, so not() returns false
        var out = ItemList.empty;
        if (input.len != 1) return out;
        if (!itemIsBool(ctx, input[0])) {
            // Non-boolean singleton is truthy, so not() = false
            try out.append(ctx.allocator, makeBoolItem(ctx, false));
            return out;
        }
        try out.append(ctx.allocator, makeBoolItem(ctx, !itemBoolValue(ctx, input[0])));
        return out;
    }
    if (std.mem.eql(u8, call.name, "is")) {
        if (call.args.len != 1) return error.InvalidFunction;
        const type_name = try typeNameFromExpr(ctx, call.args[0]);
        defer ctx.allocator.free(type_name);
        var out = ItemList.empty;
        if (input.len == 0) {
            try out.append(ctx.allocator, makeBoolItem(ctx, false));
            return out;
        }
        if (input.len != 1) return error.SingletonRequired;
        try out.append(ctx.allocator, makeBoolItem(ctx, itemIsType(ctx, input[0], type_name)));
        return out;
    }
    if (std.mem.eql(u8, call.name, "as")) {
        if (call.args.len != 1) return error.InvalidFunction;
        const type_name = try typeNameFromExpr(ctx, call.args[0]);
        defer ctx.allocator.free(type_name);
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (itemIsType(ctx, input[0], type_name)) {
            try out.append(ctx.allocator, input[0]);
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "ofType")) {
        if (call.args.len != 1) return error.InvalidFunction;
        const type_name = try typeNameFromExpr(ctx, call.args[0]);
        defer ctx.allocator.free(type_name);
        var out = ItemList.empty;
        for (input) |it| {
            if (itemIsType(ctx, it, type_name)) {
                try out.append(ctx.allocator, it);
            }
        }
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
    if (std.mem.eql(u8, call.name, "aggregate")) {
        if (call.args.len < 1 or call.args.len > 2) return error.InvalidFunction;

        var total = if (call.args.len == 2)
            try evalExpressionCtx(ctx, call.args[1], ctx.root_item, env, null)
        else
            ItemList.empty;

        if (input.len == 0) return total;

        var agg_env = Env.init(ctx.allocator);
        defer agg_env.deinit();

        if (env) |e| {
            var it = e.map.iterator();
            while (it.next()) |entry| {
                try agg_env.map.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }

        errdefer total.deinit(ctx.allocator);

        for (input, 0..) |it, idx| {
            try agg_env.map.put("$total", total.items);
            const next_total = try evalExpressionCtx(ctx, call.args[0], it, &agg_env, idx);
            total.deinit(ctx.allocator);
            total = next_total;
        }

        return total;
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
    // String matching functions: startsWith, endsWith, contains
    if (std.mem.eql(u8, call.name, "startsWith")) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty; // empty input yields empty
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const prefix = try evalStringArg(ctx, call.args[0], env);
        if (prefix == null) return ItemList.empty; // empty argument yields empty
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, std.mem.startsWith(u8, str, prefix.?)));
        return out;
    }
    if (std.mem.eql(u8, call.name, "endsWith")) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const suffix = try evalStringArg(ctx, call.args[0], env);
        if (suffix == null) return ItemList.empty;
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, std.mem.endsWith(u8, str, suffix.?)));
        return out;
    }
    if (std.mem.eql(u8, call.name, "contains")) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const needle = try evalStringArg(ctx, call.args[0], env);
        if (needle == null) return ItemList.empty;
        var out = ItemList.empty;
        const found = if (needle.?.len == 0) true else std.mem.indexOf(u8, str, needle.?) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, found));
        return out;
    }
    // substring(start [, length])
    if (std.mem.eql(u8, call.name, "substring")) {
        if (call.args.len < 1 or call.args.len > 2) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty; // empty input yields empty
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const start_opt = try evalIntegerArg(ctx, call.args[0], env);
        if (start_opt == null) return ItemList.empty; // empty start yields empty
        const start = start_opt.?;
        // Negative start or start >= string length returns empty collection
        // Special case: empty string with start=0 returns the empty string
        if (start < 0) return ItemList.empty;
        const str_len: i64 = @intCast(str.len);
        if (str_len == 0 and start == 0) {
            var out = ItemList.empty;
            try out.append(ctx.allocator, makeStringItem(ctx, ""));
            return out;
        }
        if (start >= str_len) return ItemList.empty;
        const start_usize: usize = @intCast(start);
        // Length argument
        var result_len: usize = str.len - start_usize;
        if (call.args.len == 2) {
            const len_opt = try evalIntegerArg(ctx, call.args[1], env);
            if (len_opt == null) {
                // empty length treated as omitted (take rest)
            } else if (len_opt.? <= 0) {
                // zero or negative length returns empty string
                var out = ItemList.empty;
                try out.append(ctx.allocator, makeStringItem(ctx, ""));
                return out;
            } else {
                const req_len: usize = @intCast(len_opt.?);
                if (req_len < result_len) result_len = req_len;
            }
        }
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeStringItem(ctx, str[start_usize .. start_usize + result_len]));
        return out;
    }
    // Math functions
    if (std.mem.eql(u8, call.name, "abs")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalAbs(ctx, input[0]);
    }
    if (std.mem.eql(u8, call.name, "ceiling")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalCeiling(ctx, input[0]);
    }
    if (std.mem.eql(u8, call.name, "floor")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalFloor(ctx, input[0]);
    }
    if (std.mem.eql(u8, call.name, "truncate")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalTruncate(ctx, input[0]);
    }
    if (std.mem.eql(u8, call.name, "round")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const precision: i64 = if (call.args.len > 0) (try parseIntegerArg(call)) else 0;
        return evalRound(ctx, input[0], precision);
    }
    if (std.mem.eql(u8, call.name, "exp")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalExp(ctx, input[0]);
    }
    if (std.mem.eql(u8, call.name, "ln")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalLn(ctx, input[0]);
    }
    if (std.mem.eql(u8, call.name, "log")) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const base_opt = try evalDecimalArg(ctx, call.args[0], env);
        if (base_opt == null) return ItemList.empty; // empty base yields empty
        return evalLog(ctx, input[0], base_opt.?);
    }
    if (std.mem.eql(u8, call.name, "sqrt")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalSqrt(ctx, input[0]);
    }
    if (std.mem.eql(u8, call.name, "power")) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const exp_opt = try evalDecimalArg(ctx, call.args[0], env);
        if (exp_opt == null) return ItemList.empty; // empty exponent yields empty
        return evalPower(ctx, input[0], exp_opt.?);
    }
    // iif(criterion, true-result [, otherwise-result])
    if (std.mem.eql(u8, call.name, "iif")) {
        if (call.args.len < 2 or call.args.len > 3) return error.InvalidFunction;
        // When called as method, input must be singleton or empty
        if (input.len > 1) return error.SingletonRequired;
        
        // Context item for evaluating arguments (use first input item, or empty context)
        const context_item: item.Item = if (input.len > 0) input[0] else item.Item{
            .data_kind = .none,
            .value_kind = .empty,
            .type_id = 0,
            .source_pos = 0,
            .source_end = 0,
            .node = null,
            .value = .{ .empty = {} },
        };
        
        // Evaluate criterion
        var criterion_result = try evalExpressionCtx(ctx, call.args[0], context_item, env, null);
        defer criterion_result.deinit(ctx.allocator);
        
        // Criterion must be empty, singleton boolean, or error
        if (criterion_result.items.len > 1) return error.SingletonRequired;
        
        var criterion_is_true = false;
        if (criterion_result.items.len == 1) {
            const crit_item = criterion_result.items[0];
            if (!itemIsBool(ctx, crit_item)) return error.InvalidPredicate; // non-boolean criterion
            criterion_is_true = itemBoolValue(ctx, crit_item);
        }
        // Empty criterion is treated as false
        
        // Short-circuit evaluation: only evaluate the appropriate branch
        if (criterion_is_true) {
            // Evaluate and return true-result
            return evalExpressionCtx(ctx, call.args[1], context_item, env, null);
        } else {
            // Evaluate and return otherwise-result (or empty if not provided)
            if (call.args.len == 3) {
                return evalExpressionCtx(ctx, call.args[2], context_item, env, null);
            } else {
                return ItemList.empty;
            }
        }
    }
    // String manipulation functions
    if (std.mem.eql(u8, call.name, "length")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(str.len)));
        return out;
    }
    if (std.mem.eql(u8, call.name, "indexOf")) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const needle = try evalStringArg(ctx, call.args[0], env);
        if (needle == null) return ItemList.empty; // empty substring arg yields empty
        var out = ItemList.empty;
        const idx: i64 = if (needle.?.len == 0)
            0 // empty substring returns 0
        else if (std.mem.indexOf(u8, str, needle.?)) |i|
            @intCast(i)
        else
            -1;
        try out.append(ctx.allocator, makeIntegerItem(ctx, idx));
        return out;
    }
    if (std.mem.eql(u8, call.name, "lastIndexOf")) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const needle = try evalStringArg(ctx, call.args[0], env);
        if (needle == null) return ItemList.empty;
        var out = ItemList.empty;
        const idx: i64 = if (needle.?.len == 0)
            @intCast(str.len) // empty substring returns length
        else if (std.mem.lastIndexOf(u8, str, needle.?)) |i|
            @intCast(i)
        else
            -1;
        try out.append(ctx.allocator, makeIntegerItem(ctx, idx));
        return out;
    }
    if (std.mem.eql(u8, call.name, "upper")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const upper_str = try ctx.allocator.alloc(u8, str.len);
        for (str, 0..) |c, i| {
            upper_str[i] = std.ascii.toUpper(c);
        }
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeStringItem(ctx, upper_str));
        return out;
    }
    if (std.mem.eql(u8, call.name, "lower")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const lower_str = try ctx.allocator.alloc(u8, str.len);
        for (str, 0..) |c, i| {
            lower_str[i] = std.ascii.toLower(c);
        }
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeStringItem(ctx, lower_str));
        return out;
    }
    if (std.mem.eql(u8, call.name, "replace")) {
        if (call.args.len != 2) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const pattern = try evalStringArg(ctx, call.args[0], env);
        const substitution = try evalStringArg(ctx, call.args[1], env);
        if (pattern == null or substitution == null) return ItemList.empty;
        // Replace all occurrences
        const replaced = try replaceAll(ctx.allocator, str, pattern.?, substitution.?);
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeStringItem(ctx, replaced));
        return out;
    }
    if (std.mem.eql(u8, call.name, "trim")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const trimmed = std.mem.trim(u8, str, " \t\n\r");
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeStringItem(ctx, trimmed));
        return out;
    }
    if (std.mem.eql(u8, call.name, "toChars")) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        var out = ItemList.empty;
        for (str) |c| {
            const char_str = try ctx.allocator.alloc(u8, 1);
            char_str[0] = c;
            try out.append(ctx.allocator, makeStringItem(ctx, char_str));
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "split")) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const sep = try evalStringArg(ctx, call.args[0], env);
        if (sep == null) return ItemList.empty;
        var out = ItemList.empty;
        if (sep.?.len == 0) {
            // Empty separator: split into individual characters
            for (str) |c| {
                const char_str = try ctx.allocator.alloc(u8, 1);
                char_str[0] = c;
                try out.append(ctx.allocator, makeStringItem(ctx, char_str));
            }
        } else {
            // Split by separator, keeping empty segments
            var iter = std.mem.splitSequence(u8, str, sep.?);
            while (iter.next()) |part| {
                try out.append(ctx.allocator, makeStringItem(ctx, part));
            }
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "join")) {
        if (call.args.len > 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        // Get separator (default to empty string)
        const sep = if (call.args.len == 1) (try evalStringArg(ctx, call.args[0], env)) orelse "" else "";
        // Collect all string values
        var parts = std.ArrayList([]const u8).empty;
        defer parts.deinit(ctx.allocator);
        for (input) |it| {
            const s = itemStringValue(ctx, it);
            if (s != null) try parts.append(ctx.allocator, s.?);
        }
        // Join
        const joined = try std.mem.join(ctx.allocator, sep, parts.items);
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeStringItem(ctx, joined));
        return out;
    }
    // Collection combining functions
    if (std.mem.eql(u8, call.name, "union")) {
        if (call.args.len != 1) return error.InvalidFunction;
        var other = try evalCollectionArg(ctx, call.args[0], env);
        defer other.deinit(ctx.allocator);
        // Collect all items from both collections, then deduplicate
        var combined = ItemList.empty;
        try combined.appendSlice(ctx.allocator, input);
        try combined.appendSlice(ctx.allocator, other.items);
        defer combined.deinit(ctx.allocator);
        return distinctItems(ctx, combined.items);
    }
    if (std.mem.eql(u8, call.name, "combine")) {
        if (call.args.len < 1 or call.args.len > 2) return error.InvalidFunction;
        var other = try evalCollectionArg(ctx, call.args[0], env);
        defer other.deinit(ctx.allocator);
        // Merge without deduplication
        var out = ItemList.empty;
        try out.appendSlice(ctx.allocator, input);
        try out.appendSlice(ctx.allocator, other.items);
        return out;
    }
    if (std.mem.eql(u8, call.name, "intersect")) {
        if (call.args.len != 1) return error.InvalidFunction;
        var other = try evalCollectionArg(ctx, call.args[0], env);
        defer other.deinit(ctx.allocator);
        // Find items in both collections (using equals), deduplicate result
        var matched = ItemList.empty;
        defer matched.deinit(ctx.allocator);
        for (input) |it| {
            for (other.items) |other_it| {
                if (itemsEqual(ctx, it, other_it)) {
                    try matched.append(ctx.allocator, it);
                    break;
                }
            }
        }
        return distinctItems(ctx, matched.items);
    }
    if (std.mem.eql(u8, call.name, "exclude")) {
        if (call.args.len != 1) return error.InvalidFunction;
        var other = try evalCollectionArg(ctx, call.args[0], env);
        defer other.deinit(ctx.allocator);
        // Return items NOT in other (preserves duplicates, preserves order)
        var out = ItemList.empty;
        for (input) |it| {
            var in_other = false;
            for (other.items) |other_it| {
                if (itemsEqual(ctx, it, other_it)) {
                    in_other = true;
                    break;
                }
            }
            if (!in_other) try out.append(ctx.allocator, it);
        }
        return out;
    }
    return error.InvalidFunction;
}

fn parseIntegerArg(call: ast.FunctionCall) EvalError!i64 {
    if (call.args.len != 1) return error.InvalidFunction;
    return integerLiteral(call.args[0]) orelse error.InvalidFunction;
}

fn typeNameFromExpr(ctx: anytype, expr: ast.Expr) EvalError![]const u8 {
    switch (expr) {
        .Path => |p| {
            if (p.root != .This) return error.InvalidFunction;
            var out = std.ArrayList(u8).empty;
            errdefer out.deinit(ctx.allocator);
            var wrote = false;
            for (p.steps) |step| {
                switch (step) {
                    .Property => |name| {
                        if (wrote) try out.append(ctx.allocator, '.');
                        try out.appendSlice(ctx.allocator, name);
                        wrote = true;
                    },
                    else => return error.InvalidFunction,
                }
            }
            if (!wrote) return error.InvalidFunction;
            return out.toOwnedSlice(ctx.allocator);
        },
        .Literal => |lit| switch (lit) {
            .String => |s| return ctx.allocator.dupe(u8, s),
            else => return error.InvalidFunction,
        },
        else => return error.InvalidFunction,
    }
}

fn itemStringValue(ctx: anytype, it: item.Item) ?[]const u8 {
    const val = itemToValue(ctx, it);
    return switch (val) {
        .string => |s| s,
        else => null,
    };
}

fn evalStringArg(ctx: anytype, expr: ast.Expr, env: ?*Env) EvalError!?[]const u8 {
    // For string literal, return directly
    switch (expr) {
        .Literal => |lit| switch (lit) {
            .String => |s| return s,
            else => {},
        },
        else => {},
    }
    // Evaluate the expression and get string value
    var result = try evalExpressionCtx(ctx, expr, makeEmptyItem(ctx), env, null);
    defer result.deinit(ctx.allocator);
    if (result.items.len == 0) return null;
    return itemStringValue(ctx, result.items[0]);
}

fn evalIntegerArg(ctx: anytype, expr: ast.Expr, env: ?*Env) EvalError!?i64 {
    // For integer literal, return directly
    if (integerLiteral(expr)) |i| return i;
    // Evaluate the expression and get integer value
    var result = try evalExpressionCtx(ctx, expr, makeEmptyItem(ctx), env, null);
    defer result.deinit(ctx.allocator);
    if (result.items.len == 0) return null;
    const val = itemToValue(ctx, result.items[0]);
    return switch (val) {
        .integer => |i| i,
        else => null,
    };
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

fn evalDecimalArg(ctx: anytype, expr: ast.Expr, env: ?*Env) EvalError!?f64 {
    // For number literal, return directly
    switch (expr) {
        .Literal => |lit| switch (lit) {
            .Number => |text| return std.fmt.parseFloat(f64, text) catch null,
            else => {},
        },
        else => {},
    }
    // Evaluate the expression and get numeric value
    var result = try evalExpressionCtx(ctx, expr, makeEmptyItem(ctx), env, null);
    defer result.deinit(ctx.allocator);
    if (result.items.len == 0) return null;
    return itemToFloat(ctx, result.items[0]);
}

fn evalCollectionArg(ctx: anytype, expr: ast.Expr, env: ?*Env) EvalError!ItemList {
    // Evaluate the expression with the root context and return the full result as a collection
    return evalExpressionCtx(ctx, expr, ctx.root_item, env, null);
}

fn sliceItems(ctx: anytype, values: []const item.Item) EvalError!ItemList {
    var out = ItemList.empty;
    if (values.len == 0) return out;
    try out.appendSlice(ctx.allocator, values);
    return out;
}

fn replaceAll(allocator: std.mem.Allocator, str: []const u8, pattern: []const u8, substitution: []const u8) ![]const u8 {
    if (pattern.len == 0) {
        // Empty pattern: insert substitution before each char and at end
        // Result: substitution + (char + substitution) * len(str)
        const new_len = substitution.len * (str.len + 1) + str.len;
        const result = try allocator.alloc(u8, new_len);
        var dst_idx: usize = 0;
        @memcpy(result[dst_idx..][0..substitution.len], substitution);
        dst_idx += substitution.len;
        for (str) |c| {
            result[dst_idx] = c;
            dst_idx += 1;
            @memcpy(result[dst_idx..][0..substitution.len], substitution);
            dst_idx += substitution.len;
        }
        return result;
    }
    // Count occurrences to pre-allocate
    var count: usize = 0;
    var i: usize = 0;
    while (i <= str.len - pattern.len) : (i += 1) {
        if (std.mem.eql(u8, str[i..][0..pattern.len], pattern)) {
            count += 1;
            i += pattern.len - 1; // -1 because loop will add 1
        }
    }
    if (count == 0) return str;
    // Allocate result
    const new_len = str.len - (count * pattern.len) + (count * substitution.len);
    const result = try allocator.alloc(u8, new_len);
    var src_idx: usize = 0;
    var dst_idx: usize = 0;
    while (src_idx < str.len) {
        if (src_idx + pattern.len <= str.len and std.mem.eql(u8, str[src_idx..][0..pattern.len], pattern)) {
            @memcpy(result[dst_idx..][0..substitution.len], substitution);
            dst_idx += substitution.len;
            src_idx += pattern.len;
        } else {
            result[dst_idx] = str[src_idx];
            dst_idx += 1;
            src_idx += 1;
        }
    }
    return result;
}

fn distinctItems(ctx: anytype, input: []const item.Item) EvalError!ItemList {
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

fn itemsEqual(ctx: anytype, a: item.Item, b: item.Item) bool {
    if (a.data_kind == .value and b.data_kind == .value) {
        return valueEqual(a.value.?, b.value.?);
    }
    if (a.data_kind == .node_ref and b.data_kind == .node_ref and a.node != null and b.node != null) {
        const A = @TypeOf(ctx.adapter.*);
        const a_ref = nodeRefFromRaw(A, a.node.?);
        const b_ref = nodeRefFromRaw(A, b.node.?);
        return nodeEqual(ctx, a_ref, b_ref);
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

fn nodeEqual(ctx: anytype, a_ref: @TypeOf(ctx.adapter.*).NodeRef, b_ref: @TypeOf(ctx.adapter.*).NodeRef) bool {
    const A = @TypeOf(ctx.adapter.*);
    if (rawFromNodeRef(A, a_ref) == rawFromNodeRef(A, b_ref)) return true;
    const kind_a = A.kind(ctx.adapter, a_ref);
    const kind_b = A.kind(ctx.adapter, b_ref);
    if (kind_a != kind_b) {
        if (kind_a == .number and kind_b == .number) {
            return numberEqual(A.numberText(ctx.adapter, a_ref), A.numberText(ctx.adapter, b_ref));
        }
        return false;
    }
    return switch (kind_a) {
        .null => true,
        .bool => A.boolean(ctx.adapter, a_ref) == A.boolean(ctx.adapter, b_ref),
        .number => numberEqual(A.numberText(ctx.adapter, a_ref), A.numberText(ctx.adapter, b_ref)),
        .string => std.mem.eql(u8, A.string(ctx.adapter, a_ref), A.string(ctx.adapter, b_ref)),
        .array => blk: {
            const len = A.arrayLen(ctx.adapter, a_ref);
            if (len != A.arrayLen(ctx.adapter, b_ref)) break :blk false;
            for (0..len) |i| {
                if (!nodeEqual(ctx, A.arrayAt(ctx.adapter, a_ref, i), A.arrayAt(ctx.adapter, b_ref, i))) break :blk false;
            }
            break :blk true;
        },
        .object => blk: {
            if (A.objectCount(ctx.adapter, a_ref) != A.objectCount(ctx.adapter, b_ref)) break :blk false;
            var it = A.objectIter(ctx.adapter, a_ref);
            while (it.next()) |entry| {
                const other = A.objectGet(ctx.adapter, b_ref, entry.key) orelse break :blk false;
                if (!nodeEqual(ctx, entry.value, other)) break :blk false;
            }
            break :blk true;
        },
    };
}

fn numberEqual(a: []const u8, b: []const u8) bool {
    const na = std.fmt.parseFloat(f64, a) catch return std.mem.eql(u8, a, b);
    const nb = std.fmt.parseFloat(f64, b) catch return std.mem.eql(u8, a, b);
    return na == nb;
}

fn literalToItem(ctx: anytype, lit: ast.Literal) item.Item {
    return switch (lit) {
        .Null => makeEmptyItem(ctx),
        .Bool => |b| makeBoolItem(ctx, b),
        .String => |s| makeStringItem(ctx, s),
        .Number => |n| makeNumberItem(ctx, n),
        .Quantity => |q| makeQuantityItem(ctx, q.value, q.unit),
        .Date => |d| makeDateItem(ctx, d),
        .DateTime => |d| makeDateTimeItem(ctx, d),
        .Time => |t| makeTimeItem(ctx, t),
    };
}

fn makeNodeItem(ctx: anytype, node_ref: @TypeOf(ctx.adapter.*).NodeRef, type_id_override: u32) !item.Item {
    const A = @TypeOf(ctx.adapter.*);
    const type_id = if (type_id_override != 0) type_id_override else try typeIdForNode(ctx, node_ref);
    var source_pos: u32 = 0;
    var source_end: u32 = 0;
    if (node.hasSpanSupport(A)) {
        const span = A.span(ctx.adapter, node_ref);
        source_pos = span.pos;
        source_end = span.end;
    }
    return .{
        .data_kind = .node_ref,
        .value_kind = .empty,
        .type_id = type_id,
        .source_pos = source_pos,
        .source_end = source_end,
        .node = rawFromNodeRef(A, node_ref),
        .value = null,
    };
}

fn itemFromNode(ctx: anytype, node_ref: @TypeOf(ctx.adapter.*).NodeRef) !item.Item {
    return makeNodeItem(ctx, node_ref, 0);
}

fn itemFromNodeWithType(ctx: anytype, node_ref: @TypeOf(ctx.adapter.*).NodeRef, type_id: u32) !item.Item {
    return makeNodeItem(ctx, node_ref, type_id);
}

fn makeEmptyItem(ctx: anytype) item.Item {
    _ = ctx;
    return .{
        .data_kind = .value,
        .value_kind = .empty,
        .type_id = 0,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .empty = {} },
    };
}

fn makeBoolItem(ctx: anytype, v: bool) item.Item {
    const type_id = ctx.types.getOrAdd("System.Boolean") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .boolean,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .boolean = v },
    };
}

fn makeIntegerItem(ctx: anytype, v: i64) item.Item {
    const type_id = ctx.types.getOrAdd("System.Integer") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .integer,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .integer = v },
    };
}

fn makeNumberItem(ctx: anytype, raw: []const u8) item.Item {
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
        .node = null,
        .value = .{ .decimal = raw },
    };
}

fn makeStringItem(ctx: anytype, s: []const u8) item.Item {
    const type_id = ctx.types.getOrAdd("System.String") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .string,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .string = s },
    };
}

fn makeDateItem(ctx: anytype, s: []const u8) item.Item {
    const type_id = ctx.types.getOrAdd("System.Date") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .date,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .date = s },
    };
}

fn makeDateTimeItem(ctx: anytype, s: []const u8) item.Item {
    const type_id = ctx.types.getOrAdd("System.DateTime") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .dateTime,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .dateTime = s },
    };
}

fn makeTimeItem(ctx: anytype, s: []const u8) item.Item {
    const type_id = ctx.types.getOrAdd("System.Time") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .time,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .time = s },
    };
}

fn makeQuantityItem(ctx: anytype, value: []const u8, unit: []const u8) item.Item {
    const type_id = ctx.types.getOrAdd("System.Quantity") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .quantity,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .quantity = .{ .value = value, .unit = unit } },
    };
}

fn itemIsBool(ctx: anytype, it: item.Item) bool {
    if (it.data_kind == .value and it.value != null and it.value.? == .boolean) return true;
    if (it.data_kind == .node_ref and it.node != null) {
        const A = @TypeOf(ctx.adapter.*);
        const ref = nodeRefFromRaw(A, it.node.?);
        return A.kind(ctx.adapter, ref) == .bool;
    }
    return false;
}

fn itemBoolValue(ctx: anytype, it: item.Item) bool {
    if (it.data_kind == .value and it.value != null and it.value.? == .boolean) return it.value.?.boolean;
    if (it.data_kind == .node_ref and it.node != null) {
        const A = @TypeOf(ctx.adapter.*);
        const ref = nodeRefFromRaw(A, it.node.?);
        return A.boolean(ctx.adapter, ref);
    }
    return false;
}

fn itemToValue(ctx: anytype, it: item.Item) item.Value {
    if (it.data_kind == .value and it.value != null) return it.value.?;
    if (it.data_kind == .node_ref and it.node != null) {
        const A = @TypeOf(ctx.adapter.*);
        const ref = nodeRefFromRaw(A, it.node.?);
        return switch (A.kind(ctx.adapter, ref)) {
            .null => .{ .empty = {} },
            .bool => .{ .boolean = A.boolean(ctx.adapter, ref) },
            .number => .{ .decimal = A.numberText(ctx.adapter, ref) },
            .string => .{ .string = A.string(ctx.adapter, ref) },
            else => .{ .empty = {} },
        };
    }
    return .{ .empty = {} };
}

fn typeIdForNode(ctx: anytype, node_ref: @TypeOf(ctx.adapter.*).NodeRef) !u32 {
    const A = @TypeOf(ctx.adapter.*);
    return switch (A.kind(ctx.adapter, node_ref)) {
        .null => ctx.types.getOrAdd("System.Any"),
        .bool => ctx.types.getOrAdd("System.Boolean"),
        .number => if (isInteger(A.numberText(ctx.adapter, node_ref))) ctx.types.getOrAdd("System.Integer") else ctx.types.getOrAdd("System.Decimal"),
        .string => {
            const s = A.string(ctx.adapter, node_ref);
            if (isDateTime(s)) return ctx.types.getOrAdd("System.DateTime");
            if (isDate(s)) return ctx.types.getOrAdd("System.Date");
            if (isTime(s)) return ctx.types.getOrAdd("System.Time");
            return ctx.types.getOrAdd("System.String");
        },
        .object => {
            if (resourceTypeName(ctx, node_ref)) |rt| {
                if (ctx.schema) |s| {
                    if (s.typeIdByLocalName(rt)) |tid| return tid;
                }
            }
            return ctx.types.getOrAdd("System.Any");
        },
        .array => ctx.types.getOrAdd("System.Any"),
    };
}

fn resourceTypeName(ctx: anytype, node_ref: @TypeOf(ctx.adapter.*).NodeRef) ?[]const u8 {
    const A = @TypeOf(ctx.adapter.*);
    if (A.kind(ctx.adapter, node_ref) != .object) return null;
    if (A.objectGet(ctx.adapter, node_ref, "resourceType")) |value_ref| {
        if (A.kind(ctx.adapter, value_ref) == .string) {
            return A.string(ctx.adapter, value_ref);
        }
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

// ============================================================================
// Math function implementations
// ============================================================================

fn itemToFloat(ctx: anytype, it: item.Item) ?f64 {
    const val = itemToValue(ctx, it);
    return switch (val) {
        .integer => |i| @floatFromInt(i),
        .decimal => |s| std.fmt.parseFloat(f64, s) catch null,
        else => null,
    };
}

fn itemIsInteger(ctx: anytype, it: item.Item) bool {
    const val = itemToValue(ctx, it);
    return val == .integer;
}

fn itemIntegerValue(ctx: anytype, it: item.Item) ?i64 {
    const val = itemToValue(ctx, it);
    return switch (val) {
        .integer => |i| i,
        else => null,
    };
}

fn makeDecimalItem(ctx: anytype, v: f64) EvalError!item.Item {
    // Format decimal value to string
    var buf: [64]u8 = undefined;
    const str = formatDecimal(&buf, v);
    const owned = try ctx.allocator.dupe(u8, str);
    const type_id = ctx.types.getOrAdd("System.Decimal") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .decimal,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .decimal = owned },
    };
}

fn formatDecimal(buf: []u8, v: f64) []const u8 {
    // Check if value is a whole number
    const int_val: i64 = @intFromFloat(v);
    const diff = v - @as(f64, @floatFromInt(int_val));
    if (@abs(diff) < 1e-10) {
        // Format as integer with .0 suffix for decimal type
        return std.fmt.bufPrint(buf, "{d}.0", .{int_val}) catch "0.0";
    }
    // General decimal formatting
    return std.fmt.bufPrint(buf, "{d}", .{v}) catch "0.0";
}

fn evalAbs(ctx: anytype, it: item.Item) EvalError!ItemList {
    var out = ItemList.empty;
    const val = itemToValue(ctx, it);
    switch (val) {
        .integer => |i| {
            const abs_val = if (i < 0) -i else i;
            try out.append(ctx.allocator, makeIntegerItem(ctx, abs_val));
        },
        .decimal => |s| {
            const f = std.fmt.parseFloat(f64, s) catch return out;
            const abs_val = @abs(f);
            try out.append(ctx.allocator, try makeDecimalItem(ctx, abs_val));
        },
        else => {}, // return empty for non-numeric types
    }
    return out;
}

fn evalCeiling(ctx: anytype, it: item.Item) EvalError!ItemList {
    var out = ItemList.empty;
    const val = itemToValue(ctx, it);
    switch (val) {
        .integer => |i| {
            // Integer unchanged
            try out.append(ctx.allocator, makeIntegerItem(ctx, i));
        },
        .decimal => |s| {
            const f = std.fmt.parseFloat(f64, s) catch return out;
            const ceil_val: i64 = @intFromFloat(@ceil(f));
            try out.append(ctx.allocator, makeIntegerItem(ctx, ceil_val));
        },
        else => {}, // return empty for non-numeric types
    }
    return out;
}

fn evalFloor(ctx: anytype, it: item.Item) EvalError!ItemList {
    var out = ItemList.empty;
    const val = itemToValue(ctx, it);
    switch (val) {
        .integer => |i| {
            // Integer unchanged
            try out.append(ctx.allocator, makeIntegerItem(ctx, i));
        },
        .decimal => |s| {
            const f = std.fmt.parseFloat(f64, s) catch return out;
            const floor_val: i64 = @intFromFloat(@floor(f));
            try out.append(ctx.allocator, makeIntegerItem(ctx, floor_val));
        },
        else => {}, // return empty for non-numeric types
    }
    return out;
}

fn evalTruncate(ctx: anytype, it: item.Item) EvalError!ItemList {
    var out = ItemList.empty;
    const val = itemToValue(ctx, it);
    switch (val) {
        .integer => |i| {
            // Integer unchanged
            try out.append(ctx.allocator, makeIntegerItem(ctx, i));
        },
        .decimal => |s| {
            const f = std.fmt.parseFloat(f64, s) catch return out;
            const trunc_val: i64 = @intFromFloat(@trunc(f));
            try out.append(ctx.allocator, makeIntegerItem(ctx, trunc_val));
        },
        else => {}, // return empty for non-numeric types
    }
    return out;
}

fn evalRound(ctx: anytype, it: item.Item, precision: i64) EvalError!ItemList {
    var out = ItemList.empty;
    if (precision < 0) return error.InvalidFunction; // negative precision is an error

    const val = itemToValue(ctx, it);
    const f: f64 = switch (val) {
        .integer => |i| @floatFromInt(i),
        .decimal => |s| std.fmt.parseFloat(f64, s) catch return out,
        else => return out, // return empty for non-numeric types
    };

    // Round to specified precision
    const scale = std.math.pow(f64, 10.0, @floatFromInt(precision));
    const rounded = @round(f * scale) / scale;

    if (precision == 0) {
        // Return integer when precision is 0
        const int_val: i64 = @intFromFloat(rounded);
        try out.append(ctx.allocator, makeIntegerItem(ctx, int_val));
    } else {
        try out.append(ctx.allocator, try makeDecimalItem(ctx, rounded));
    }
    return out;
}

fn evalExp(ctx: anytype, it: item.Item) EvalError!ItemList {
    var out = ItemList.empty;
    const f = itemToFloat(ctx, it) orelse return out;
    const result = @exp(f);
    try out.append(ctx.allocator, try makeDecimalItem(ctx, result));
    return out;
}

fn evalLn(ctx: anytype, it: item.Item) EvalError!ItemList {
    var out = ItemList.empty;
    const f = itemToFloat(ctx, it) orelse return out;
    if (f <= 0) return out; // domain error returns empty
    const result = @log(f);
    try out.append(ctx.allocator, try makeDecimalItem(ctx, result));
    return out;
}

fn evalLog(ctx: anytype, it: item.Item, base: f64) EvalError!ItemList {
    var out = ItemList.empty;
    const f = itemToFloat(ctx, it) orelse return out;
    if (f <= 0 or base <= 0 or base == 1) return out; // domain errors return empty
    const result = @log(f) / @log(base);
    try out.append(ctx.allocator, try makeDecimalItem(ctx, result));
    return out;
}

fn evalSqrt(ctx: anytype, it: item.Item) EvalError!ItemList {
    var out = ItemList.empty;
    const f = itemToFloat(ctx, it) orelse return out;
    if (f < 0) return out; // domain error returns empty
    const result = @sqrt(f);
    try out.append(ctx.allocator, try makeDecimalItem(ctx, result));
    return out;
}

fn evalPower(ctx: anytype, it: item.Item, exponent: f64) EvalError!ItemList {
    var out = ItemList.empty;
    const base = itemToFloat(ctx, it) orelse return out;
    // Check for domain errors (negative base with fractional exponent)
    if (base < 0 and @mod(exponent, 1.0) != 0) return out; // returns empty
    const result = std.math.pow(f64, base, exponent);
    // Check if result is valid (not NaN or infinite)
    if (std.math.isNan(result) or std.math.isInf(result)) return out;
    
    // If base was integer and exponent is non-negative integer, return integer
    if (itemIsInteger(ctx, it) and exponent >= 0 and @mod(exponent, 1.0) == 0) {
        const int_result: i64 = @intFromFloat(result);
        try out.append(ctx.allocator, makeIntegerItem(ctx, int_result));
    } else {
        try out.append(ctx.allocator, try makeDecimalItem(ctx, result));
    }
    return out;
}
