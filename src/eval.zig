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
        self.items.deinit(self.doc.arena.allocator());
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
    var ctx = EvalContext(JsonDocAdapter){
        .allocator = doc.arena.allocator(),
        .adapter = &adapter,
        .types = types,
        .schema = schema_ptr,
    };
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
    errdefer current.deinit(ctx.allocator);

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
                errdefer next.deinit(ctx.allocator);
                for (current.items) |it| {
                    try applySegment(ctx, it, name, &next);
                }
                current.deinit(ctx.allocator);
                current = next;
                next = ItemList.empty;
            },
            .Index => |idx| {
                var next = ItemList.empty;
                errdefer next.deinit(ctx.allocator);
                if (idx < current.items.len) {
                    try next.append(ctx.allocator, current.items[idx]);
                }
                current.deinit(ctx.allocator);
                current = next;
                next = ItemList.empty;
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
    if (left.len == 1) {
        try out.append(ctx.allocator, makeBoolItem(ctx, itemsEquivalent(ctx, left[0], right[0])));
        return out;
    }

    // Multi-item collections compare without regard to order (cardinality must match).
    const used = try ctx.allocator.alloc(bool, right.len);
    defer ctx.allocator.free(used);
    @memset(used, false);

    for (left) |l| {
        var matched = false;
        for (right, 0..) |r, i| {
            if (used[i]) continue;
            if (itemsEquivalent(ctx, l, r)) {
                used[i] = true;
                matched = true;
                break;
            }
        }
        if (!matched) {
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

fn isWhitespaceChar(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

fn stringEquivalent(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, 0..) |ca, idx| {
        const cb = b[idx];
        if (isWhitespaceChar(ca) and isWhitespaceChar(cb)) continue;
        if (std.ascii.toLower(ca) != std.ascii.toLower(cb)) return false;
    }
    return true;
}

fn decimalNormScale(text: []const u8) ?u32 {
    const scale = decimalScale(text) orelse return null;
    if (scale == 0) return 0;
    var norm = scale;
    var remaining = scale;
    var i: isize = @intCast(text.len - 1);
    while (i >= 0 and remaining > 0) : (i -= 1) {
        const c = text[@intCast(i)];
        if (c == '.') continue;
        if (c != '0') break;
        norm -= 1;
        remaining -= 1;
    }
    return norm;
}

fn pow10i128(exp: u32) ?i128 {
    var result: i128 = 1;
    var i: u32 = 0;
    while (i < exp) : (i += 1) {
        const mul = @mulWithOverflow(result, 10);
        if (mul[1] != 0) return null;
        result = mul[0];
    }
    return result;
}

fn decimalRoundedScaled(text: []const u8, target_scale: u32) ?i128 {
    const scale = decimalScale(text) orelse return null;
    if (scale == target_scale) return parseDecimalScaled(text, scale);
    if (scale < target_scale) return parseDecimalScaled(text, target_scale);

    const scaled = parseDecimalScaled(text, scale) orelse return null;
    const diff: u32 = scale - target_scale;
    const div = pow10i128(diff) orelse return null;

    const negative = scaled < 0;
    const abs_val: i128 = if (negative) -scaled else scaled;
    var quotient: i128 = @divTrunc(abs_val, div);
    const remainder: i128 = @mod(abs_val, div);

    const twice = @mulWithOverflow(remainder, 2);
    if (twice[1] != 0) return null;
    if (twice[0] >= div) quotient += 1;

    return if (negative) -quotient else quotient;
}

fn decimalEquivalentText(a: []const u8, b: []const u8) bool {
    const scale_a = decimalNormScale(a) orelse return false;
    const scale_b = decimalNormScale(b) orelse return false;
    const target: u32 = if (scale_a < scale_b) scale_a else scale_b;
    const scaled_a = decimalRoundedScaled(a, target) orelse return false;
    const scaled_b = decimalRoundedScaled(b, target) orelse return false;
    return scaled_a == scaled_b;
}

fn decimalEqualText(a: []const u8, b: []const u8) bool {
    const scale_a = decimalScale(a) orelse return false;
    const scale_b = decimalScale(b) orelse return false;
    const target: u32 = if (scale_a > scale_b) scale_a else scale_b;
    const scaled_a = parseDecimalScaled(a, target) orelse return false;
    const scaled_b = parseDecimalScaled(b, target) orelse return false;
    return scaled_a == scaled_b;
}

const DateParts = struct {
    year: []const u8,
    month: ?[]const u8,
    day: ?[]const u8,
};

const TimeParts = struct {
    hour: []const u8,
    minute: []const u8,
    second: ?[]const u8,
    zone: ?[]const u8,
};

const DateTimeParts = struct {
    date: DateParts,
    time: TimeParts,
};

const TimePartsPartial = struct {
    hour: []const u8,
    minute: ?[]const u8,
    second: ?[]const u8,
    zone: ?[]const u8,
};

const SecondParts = struct {
    whole: []const u8,
    frac: ?[]const u8,
};

const BoundaryKind = enum { low, high };

const DecimalMaxPrecision: u32 = 8;
const DateMaxPrecision: u32 = 8;
const DateTimeMaxPrecision: u32 = 17;
const TimeMaxPrecision: u32 = 9;

fn parseDateParts(text: []const u8) ?DateParts {
    if (text.len == 0) return null;
    var it = std.mem.splitScalar(u8, text, '-');
    const year = it.next() orelse return null;
    const month = it.next();
    const day = it.next();
    if (it.next() != null) return null;
    return .{ .year = year, .month = month, .day = day };
}

fn parseTimeParts(text: []const u8) ?TimeParts {
    if (text.len == 0) return null;
    var tz_index: ?usize = null;
    var idx: usize = 0;
    while (idx < text.len) : (idx += 1) {
        const c = text[idx];
        if (c == 'Z' or c == '+' or c == '-') {
            tz_index = idx;
            break;
        }
    }
    const time_part = if (tz_index) |pos| text[0..pos] else text;
    const zone_part = if (tz_index) |pos| text[pos..] else null;

    var it = std.mem.splitScalar(u8, time_part, ':');
    const hour = it.next() orelse return null;
    const minute = it.next() orelse return null;
    const second = it.next();
    if (it.next() != null) return null;

    return .{
        .hour = hour,
        .minute = minute,
        .second = second,
        .zone = zone_part,
    };
}

fn parseDateTimeParts(text: []const u8) ?DateTimeParts {
    const t_index = std.mem.indexOfScalar(u8, text, 'T') orelse return null;
    const date_part = text[0..t_index];
    const time_part = text[t_index + 1 ..];
    const date = parseDateParts(date_part) orelse return null;
    const time = parseTimeParts(time_part) orelse return null;
    return .{ .date = date, .time = time };
}

fn stripAtPrefix(text: []const u8) []const u8 {
    if (text.len > 0 and text[0] == '@') return text[1..];
    return text;
}

fn stripTimePrefix(text: []const u8) []const u8 {
    if (text.len > 0 and text[0] == 'T') return text[1..];
    return text;
}

fn parseTimePartsPartial(text: []const u8) ?TimePartsPartial {
    if (text.len == 0) return null;
    var tz_index: ?usize = null;
    var idx: usize = 0;
    while (idx < text.len) : (idx += 1) {
        const c = text[idx];
        if (c == 'Z' or c == '+' or c == '-') {
            tz_index = idx;
            break;
        }
    }
    const time_part = if (tz_index) |pos| text[0..pos] else text;
    const zone_part = if (tz_index) |pos| text[pos..] else null;

    var it = std.mem.splitScalar(u8, time_part, ':');
    const hour = it.next() orelse return null;
    const minute = it.next();
    const second = it.next();
    if (it.next() != null) return null;

    return .{
        .hour = hour,
        .minute = minute,
        .second = second,
        .zone = zone_part,
    };
}

fn splitSecond(sec: []const u8) SecondParts {
    if (std.mem.indexOfScalar(u8, sec, '.')) |dot| {
        return .{ .whole = sec[0..dot], .frac = sec[dot + 1 ..] };
    }
    return .{ .whole = sec, .frac = null };
}

fn datePrecisionDigits(text: []const u8) ?u32 {
    const normalized = stripAtPrefix(text);
    const parts = parseDateParts(normalized) orelse return null;
    var prec: u32 = 4;
    if (parts.month != null) prec += 2;
    if (parts.day != null) prec += 2;
    return prec;
}

fn timePrecisionDigits(text: []const u8) ?u32 {
    const normalized = stripTimePrefix(stripAtPrefix(text));
    const parts = parseTimePartsPartial(normalized) orelse return null;
    var prec: u32 = 2;
    if (parts.minute == null) return prec;
    prec += 2;
    if (parts.second) |sec| {
        prec += 2;
        if (std.mem.indexOfScalar(u8, sec, '.')) |dot| {
            const frac_len = sec.len - dot - 1;
            prec += @intCast(frac_len);
        }
    }
    return prec;
}

fn dateTimePrecisionDigits(text: []const u8) ?u32 {
    const normalized = stripAtPrefix(text);
    if (std.mem.indexOfScalar(u8, normalized, 'T')) |t_index| {
        const date_part = normalized[0..t_index];
        const time_part = normalized[t_index + 1 ..];
        const date_prec = datePrecisionDigits(date_part) orelse return null;
        const time_prec = timePrecisionDigits(time_part) orelse return null;
        return date_prec + time_prec;
    }
    return datePrecisionDigits(normalized);
}

fn isLeapYear(year: i32) bool {
    if (@mod(year, 400) == 0) return true;
    if (@mod(year, 100) == 0) return false;
    return @mod(year, 4) == 0;
}

fn daysInMonth(year: i32, month: u32) ?u32 {
    if (month < 1 or month > 12) return null;
    return switch (month) {
        1, 3, 5, 7, 8, 10, 12 => 31,
        4, 6, 9, 11 => 30,
        2 => if (isLeapYear(year)) 29 else 28,
        else => null,
    };
}

fn appendTwoDigits(out: *std.ArrayList(u8), allocator: std.mem.Allocator, value: u32) EvalError!void {
    var buf: [2]u8 = undefined;
    const slice = std.fmt.bufPrint(&buf, "{d:0>2}", .{value}) catch return error.InvalidOperand;
    try out.appendSlice(allocator, slice);
}

fn buildDateBoundary(
    allocator: std.mem.Allocator,
    text: []const u8,
    precision: u32,
    kind: BoundaryKind,
) EvalError!?[]const u8 {
    if (precision != 4 and precision != 6 and precision != 8) return null;
    const normalized = stripAtPrefix(text);
    const parts = parseDateParts(normalized) orelse return null;

    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(allocator);

    try out.appendSlice(allocator, parts.year);

    if (precision >= 6) {
        try out.append(allocator, '-');
        const month = parts.month orelse if (kind == .low) "01" else "12";
        try out.appendSlice(allocator, month);
    }

    if (precision >= 8) {
        try out.append(allocator, '-');
        if (parts.day) |day| {
            try out.appendSlice(allocator, day);
        } else if (kind == .low) {
            try out.appendSlice(allocator, "01");
        } else {
            const month = parts.month orelse "12";
            const year_val = std.fmt.parseInt(i32, parts.year, 10) catch return null;
            const month_val = std.fmt.parseInt(u32, month, 10) catch return null;
            const day_val = daysInMonth(year_val, month_val) orelse return null;
            try appendTwoDigits(&out, allocator, day_val);
        }
    }

    return try out.toOwnedSlice(allocator);
}

fn buildTimeBoundary(
    allocator: std.mem.Allocator,
    text: []const u8,
    precision: u32,
    kind: BoundaryKind,
) EvalError!?[]const u8 {
    if (precision != 2 and precision != 4 and precision != 6 and precision != 9) return null;
    const normalized = stripTimePrefix(stripAtPrefix(text));
    const parts = parseTimePartsPartial(normalized) orelse return null;

    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(allocator);

    if (parts.hour.len == 0) return null;
    try out.appendSlice(allocator, parts.hour);

    if (precision >= 4) {
        try out.append(allocator, ':');
        const minute = parts.minute orelse if (kind == .low) "00" else "59";
        try out.appendSlice(allocator, minute);
    }

    if (precision >= 6) {
        try out.append(allocator, ':');
        const sec_text = parts.second orelse if (kind == .low) "00" else "59";
        const sec_parts = splitSecond(sec_text);
        try out.appendSlice(allocator, sec_parts.whole);
        if (precision == 9) {
            try out.append(allocator, '.');
            if (sec_parts.frac) |frac| {
                if (frac.len >= 3) {
                    try out.appendSlice(allocator, frac[0..3]);
                } else {
                    try out.appendSlice(allocator, frac);
                    const pad_char: u8 = if (kind == .low) '0' else '9';
                    var i: usize = frac.len;
                    while (i < 3) : (i += 1) {
                        try out.append(allocator, pad_char);
                    }
                }
            } else {
                const fill = if (kind == .low) "000" else "999";
                try out.appendSlice(allocator, fill);
            }
        }
    }

    if (parts.zone) |zone| {
        try out.appendSlice(allocator, zone);
    }

    return try out.toOwnedSlice(allocator);
}

fn buildDateTimeBoundary(
    allocator: std.mem.Allocator,
    text: []const u8,
    precision: u32,
    kind: BoundaryKind,
) EvalError!?[]const u8 {
    if (precision != 4 and precision != 6 and precision != 8 and precision != 10 and precision != 12 and precision != 14 and precision != 17) return null;
    const normalized = stripAtPrefix(text);
    var date_part = normalized;
    var time_part: ?[]const u8 = null;
    if (std.mem.indexOfScalar(u8, normalized, 'T')) |t_index| {
        date_part = normalized[0..t_index];
        time_part = normalized[t_index + 1 ..];
    }

    const date = parseDateParts(date_part) orelse return null;
    const time = if (time_part) |tp| parseTimePartsPartial(tp) else null;

    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(allocator);

    try out.appendSlice(allocator, date.year);

    if (precision >= 6) {
        try out.append(allocator, '-');
        const month = date.month orelse if (kind == .low) "01" else "12";
        try out.appendSlice(allocator, month);
    }

    if (precision >= 8) {
        try out.append(allocator, '-');
        if (date.day) |day| {
            try out.appendSlice(allocator, day);
        } else if (kind == .low) {
            try out.appendSlice(allocator, "01");
        } else {
            const month = date.month orelse "12";
            const year_val = std.fmt.parseInt(i32, date.year, 10) catch return null;
            const month_val = std.fmt.parseInt(u32, month, 10) catch return null;
            const day_val = daysInMonth(year_val, month_val) orelse return null;
            try appendTwoDigits(&out, allocator, day_val);
        }
    }

    if (precision >= 10) {
        const time_parts = time orelse TimePartsPartial{
            .hour = "",
            .minute = null,
            .second = null,
            .zone = null,
        };
        try out.append(allocator, 'T');
        const hour = if (time_parts.hour.len > 0) time_parts.hour else if (kind == .low) "00" else "23";
        try out.appendSlice(allocator, hour);

        if (precision >= 12) {
            try out.append(allocator, ':');
            const minute = time_parts.minute orelse if (kind == .low) "00" else "59";
            try out.appendSlice(allocator, minute);
        }

        if (precision >= 14) {
            try out.append(allocator, ':');
            const sec_text = time_parts.second orelse if (kind == .low) "00" else "59";
            const sec_parts = splitSecond(sec_text);
            try out.appendSlice(allocator, sec_parts.whole);

            if (precision == 17) {
                try out.append(allocator, '.');
                if (sec_parts.frac) |frac| {
                    if (frac.len >= 3) {
                        try out.appendSlice(allocator, frac[0..3]);
                    } else {
                        try out.appendSlice(allocator, frac);
                        const pad_char: u8 = if (kind == .low) '0' else '9';
                        var i: usize = frac.len;
                        while (i < 3) : (i += 1) {
                            try out.append(allocator, pad_char);
                        }
                    }
                } else {
                    const fill = if (kind == .low) "000" else "999";
                    try out.appendSlice(allocator, fill);
                }
            }
        }

        const zone = if (time) |tp| tp.zone else null;
        if (zone) |z| {
            try out.appendSlice(allocator, z);
        } else {
            const fill = if (kind == .low) "+14:00" else "-12:00";
            try out.appendSlice(allocator, fill);
        }
    }

    return try out.toOwnedSlice(allocator);
}

fn datePartsEquivalent(a: DateParts, b: DateParts) bool {
    if (!std.mem.eql(u8, a.year, b.year)) return false;
    if ((a.month == null) != (b.month == null)) return false;
    if (a.month) |m| {
        if (!std.mem.eql(u8, m, b.month.?)) return false;
    }
    if ((a.day == null) != (b.day == null)) return false;
    if (a.day) |d| {
        if (!std.mem.eql(u8, d, b.day.?)) return false;
    }
    return true;
}

fn timePartsEquivalent(a: TimeParts, b: TimeParts) bool {
    if ((a.zone == null) != (b.zone == null)) return false;
    if (a.zone) |z| {
        if (!std.mem.eql(u8, z, b.zone.?)) return false;
    }
    if (!std.mem.eql(u8, a.hour, b.hour)) return false;
    if (!std.mem.eql(u8, a.minute, b.minute)) return false;
    if ((a.second == null) != (b.second == null)) return false;
    if (a.second) |sec| {
        return decimalEqualText(sec, b.second.?);
    }
    return true;
}

fn dateEquivalent(a: []const u8, b: []const u8) bool {
    const pa = parseDateParts(a) orelse return false;
    const pb = parseDateParts(b) orelse return false;
    return datePartsEquivalent(pa, pb);
}

fn timeEquivalent(a: []const u8, b: []const u8) bool {
    const ta = parseTimeParts(a) orelse return false;
    const tb = parseTimeParts(b) orelse return false;
    return timePartsEquivalent(ta, tb);
}

fn dateTimeEquivalent(a: []const u8, b: []const u8) bool {
    const da = parseDateTimeParts(a) orelse return false;
    const db = parseDateTimeParts(b) orelse return false;
    if (!datePartsEquivalent(da.date, db.date)) return false;
    return timePartsEquivalent(da.time, db.time);
}

fn valueEquivalent(ctx: anytype, a_val: item.Value, a_type: u32, b_val: item.Value, b_type: u32) bool {
    if (a_val == .empty or b_val == .empty) return a_val == .empty and b_val == .empty;

    const date_id = ctx.types.getOrAdd("System.Date") catch 0;
    const time_id = ctx.types.getOrAdd("System.Time") catch 0;
    const datetime_id = ctx.types.getOrAdd("System.DateTime") catch 0;
    const string_id = ctx.types.getOrAdd("System.String") catch 0;

    const a_is_date = a_val == .date or a_type == date_id;
    const b_is_date = b_val == .date or b_type == date_id;
    if (a_is_date or b_is_date) {
        if (!(a_is_date and b_is_date)) return false;
        const a_text = if (a_val == .date) a_val.date else if (a_val == .string) a_val.string else return false;
        const b_text = if (b_val == .date) b_val.date else if (b_val == .string) b_val.string else return false;
        return dateEquivalent(a_text, b_text);
    }

    const a_is_time = a_val == .time or a_type == time_id;
    const b_is_time = b_val == .time or b_type == time_id;
    if (a_is_time or b_is_time) {
        if (!(a_is_time and b_is_time)) return false;
        const a_text = if (a_val == .time) a_val.time else if (a_val == .string) a_val.string else return false;
        const b_text = if (b_val == .time) b_val.time else if (b_val == .string) b_val.string else return false;
        return timeEquivalent(a_text, b_text);
    }

    const a_is_datetime = a_val == .dateTime or a_type == datetime_id;
    const b_is_datetime = b_val == .dateTime or b_type == datetime_id;
    if (a_is_datetime or b_is_datetime) {
        if (!(a_is_datetime and b_is_datetime)) return false;
        const a_text = if (a_val == .dateTime) a_val.dateTime else if (a_val == .string) a_val.string else return false;
        const b_text = if (b_val == .dateTime) b_val.dateTime else if (b_val == .string) b_val.string else return false;
        return dateTimeEquivalent(a_text, b_text);
    }

    const a_numeric = a_val == .integer or a_val == .decimal;
    const b_numeric = b_val == .integer or b_val == .decimal;
    if (a_numeric and b_numeric) {
        var buf_a: [64]u8 = undefined;
        var buf_b: [64]u8 = undefined;
        const text_a = switch (a_val) {
            .integer => std.fmt.bufPrint(&buf_a, "{d}", .{a_val.integer}) catch return false,
            .decimal => a_val.decimal,
            else => return false,
        };
        const text_b = switch (b_val) {
            .integer => std.fmt.bufPrint(&buf_b, "{d}", .{b_val.integer}) catch return false,
            .decimal => b_val.decimal,
            else => return false,
        };
        return decimalEquivalentText(text_a, text_b);
    }

    const a_is_string = a_val == .string or a_type == string_id;
    const b_is_string = b_val == .string or b_type == string_id;
    if (a_is_string and b_is_string) {
        const a_text = switch (a_val) {
            .string => a_val.string,
            else => return false,
        };
        const b_text = switch (b_val) {
            .string => b_val.string,
            else => return false,
        };
        return stringEquivalent(a_text, b_text);
    }

    if (a_val == .boolean and b_val == .boolean) return a_val.boolean == b_val.boolean;
    if (a_val == .quantity and b_val == .quantity) return valueEqual(a_val, b_val);

    return false;
}

fn itemsEquivalent(ctx: anytype, a: item.Item, b: item.Item) bool {
    const A = @TypeOf(ctx.adapter.*);
    const a_is_node = a.data_kind == .node_ref and a.node != null;
    const b_is_node = b.data_kind == .node_ref and b.node != null;

    if (a_is_node and b_is_node) {
        const a_ref = nodeRefFromRaw(A, a.node.?);
        const b_ref = nodeRefFromRaw(A, b.node.?);
        const kind_a = A.kind(ctx.adapter, a_ref);
        const kind_b = A.kind(ctx.adapter, b_ref);
        if (kind_a != kind_b) return false;
        return switch (kind_a) {
            .null => true,
            .bool => A.boolean(ctx.adapter, a_ref) == A.boolean(ctx.adapter, b_ref),
            .number => decimalEquivalentText(A.numberText(ctx.adapter, a_ref), A.numberText(ctx.adapter, b_ref)),
            .string => blk: {
                const text_a = A.string(ctx.adapter, a_ref);
                const text_b = A.string(ctx.adapter, b_ref);
                const date_id = ctx.types.getOrAdd("System.Date") catch 0;
                const time_id = ctx.types.getOrAdd("System.Time") catch 0;
                const datetime_id = ctx.types.getOrAdd("System.DateTime") catch 0;
                if (a.type_id == date_id and b.type_id == date_id) break :blk dateEquivalent(text_a, text_b);
                if (a.type_id == time_id and b.type_id == time_id) break :blk timeEquivalent(text_a, text_b);
                if (a.type_id == datetime_id and b.type_id == datetime_id) break :blk dateTimeEquivalent(text_a, text_b);
                break :blk stringEquivalent(text_a, text_b);
            },
            .array, .object => nodeEqual(ctx, a_ref, b_ref),
        };
    }

    if (a_is_node or b_is_node) {
        if (a_is_node) {
            const a_ref = nodeRefFromRaw(A, a.node.?);
            const kind_a = A.kind(ctx.adapter, a_ref);
            if (kind_a == .array or kind_a == .object) return false;
        }
        if (b_is_node) {
            const b_ref = nodeRefFromRaw(A, b.node.?);
            const kind_b = A.kind(ctx.adapter, b_ref);
            if (kind_b == .array or kind_b == .object) return false;
        }
    }

    return valueEquivalent(ctx, itemToValue(ctx, a), a.type_id, itemToValue(ctx, b), b.type_id);
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

    // Compare dates (lexicographic on ISO-8601 strings)
    if (va == .date and vb == .date) {
        return switch (std.mem.order(u8, va.date, vb.date)) {
            .lt => -1,
            .eq => 0,
            .gt => 1,
        };
    }

    // Compare times (lexicographic on ISO-8601 strings)
    if (va == .time and vb == .time) {
        return switch (std.mem.order(u8, va.time, vb.time)) {
            .lt => -1,
            .eq => 0,
            .gt => 1,
        };
    }

    // Compare dateTimes (lexicographic on ISO-8601 strings)
    if (va == .dateTime and vb == .dateTime) {
        return switch (std.mem.order(u8, va.dateTime, vb.dateTime)) {
            .lt => -1,
            .eq => 0,
            .gt => 1,
        };
    }

    // Compare quantities (same unit only)
    if (va == .quantity and vb == .quantity) {
        if (!std.mem.eql(u8, va.quantity.unit, vb.quantity.unit)) return null;
        const qa = std.fmt.parseFloat(f64, va.quantity.value) catch return null;
        const qb = std.fmt.parseFloat(f64, vb.quantity.value) catch return null;
        if (qa < qb) return -1;
        if (qa > qb) return 1;
        return 0;
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
    errdefer current.deinit(ctx.allocator);

    // Then apply each step
    for (inv.steps) |step| {
        switch (step) {
            .Property => |name| {
                var next = ItemList.empty;
                errdefer next.deinit(ctx.allocator);
                for (current.items) |it| {
                    try applySegment(ctx, it, name, &next);
                }
                current.deinit(ctx.allocator);
                current = next;
                next = ItemList.empty;
            },
            .Index => |idx| {
                var next = ItemList.empty;
                errdefer next.deinit(ctx.allocator);
                if (idx < current.items.len) {
                    try next.append(ctx.allocator, current.items[idx]);
                }
                current.deinit(ctx.allocator);
                current = next;
                next = ItemList.empty;
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
    if (std.mem.eql(u8, call.name, "toInteger")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToInteger(ctx, input[0])) |val| {
            try out.append(ctx.allocator, makeIntegerItem(ctx, val));
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "toBoolean")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToBoolean(ctx, input[0])) |val| {
            try out.append(ctx.allocator, makeBoolItem(ctx, val));
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "toDecimal")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToDecimalText(ctx, input[0])) |text| {
            try out.append(ctx.allocator, makeDecimalItemText(ctx, text));
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "convertsToInteger")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = convertToInteger(ctx, input[0]) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (std.mem.eql(u8, call.name, "convertsToBoolean")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = convertToBoolean(ctx, input[0]) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (std.mem.eql(u8, call.name, "convertsToDecimal")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = canConvertToDecimal(ctx, input[0]);
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (std.mem.eql(u8, call.name, "convertsToString")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = canConvertToString(ctx, input[0]);
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (std.mem.eql(u8, call.name, "toTime")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToTime(ctx, input[0])) |time_str| {
            try out.append(ctx.allocator, makeTimeItem(ctx, time_str));
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "convertsToTime")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = convertToTime(ctx, input[0]) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (std.mem.eql(u8, call.name, "toDate")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToDate(ctx, input[0])) |date_str| {
            try out.append(ctx.allocator, makeDateItem(ctx, date_str));
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "convertsToDate")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = convertToDate(ctx, input[0]) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (std.mem.eql(u8, call.name, "toDateTime")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToDateTime(ctx, input[0])) |dt_str| {
            try out.append(ctx.allocator, makeDateTimeItem(ctx, dt_str));
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "convertsToDateTime")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = convertToDateTime(ctx, input[0]) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
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
        errdefer out.deinit(ctx.allocator);
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
        errdefer out.deinit(ctx.allocator);
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
    if (std.mem.eql(u8, call.name, "repeat")) {
        if (call.args.len != 1) return error.InvalidFunction;
        var out = ItemList.empty;
        errdefer out.deinit(ctx.allocator);
        if (input.len == 0) return out;

        var queue = ItemList.empty;
        errdefer queue.deinit(ctx.allocator);
        try queue.appendSlice(ctx.allocator, input);

        var idx: usize = 0;
        while (idx < queue.items.len) : (idx += 1) {
            const current_item = queue.items[idx];
            var projection = try evalExpressionCtx(ctx, call.args[0], current_item, env, idx);
            defer projection.deinit(ctx.allocator);
            for (projection.items) |proj_item| {
                var seen = false;
                for (out.items) |existing| {
                    if (itemsEqual(ctx, proj_item, existing)) {
                        seen = true;
                        break;
                    }
                }
                if (!seen) {
                    try out.append(ctx.allocator, proj_item);
                    try queue.append(ctx.allocator, proj_item);
                }
            }
        }

        queue.deinit(ctx.allocator);
        return out;
    }
    if (std.mem.eql(u8, call.name, "repeatAll")) {
        if (call.args.len != 1) return error.InvalidFunction;
        var out = ItemList.empty;
        errdefer out.deinit(ctx.allocator);
        if (input.len == 0) return out;

        var current = ItemList.empty;
        errdefer current.deinit(ctx.allocator);
        try current.appendSlice(ctx.allocator, input);

        while (current.items.len > 0) {
            var next = ItemList.empty;
            errdefer next.deinit(ctx.allocator);
            for (current.items, 0..) |it, idx| {
                var projection = try evalExpressionCtx(ctx, call.args[0], it, env, idx);
                defer projection.deinit(ctx.allocator);
                if (projection.items.len == 0) continue;
                try out.appendSlice(ctx.allocator, projection.items);
                try next.appendSlice(ctx.allocator, projection.items);
            }
            current.deinit(ctx.allocator);
            current = next;
        }

        current.deinit(ctx.allocator);
        return out;
    }
    if (std.mem.eql(u8, call.name, "sum")) {
        if (call.args.len != 0) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;

        const integer_type_id = ctx.types.getOrAdd("System.Integer") catch 0;
        const decimal_type_id = ctx.types.getOrAdd("System.Decimal") catch 0;
        const quantity_type_id = ctx.types.getOrAdd("System.Quantity") catch 0;

        const first_kind = sumKindForItem(input[0], integer_type_id, decimal_type_id, quantity_type_id) orelse return error.InvalidOperand;

        var out = ItemList.empty;
        switch (first_kind) {
            .integer => {
                var total: i64 = 0;
                for (input) |it| {
                    if (sumKindForItem(it, integer_type_id, decimal_type_id, quantity_type_id) != .integer) {
                        return error.InvalidOperand;
                    }
                    if (it.data_kind == .value and it.value != null and it.value.? == .integer) {
                        total +%= it.value.?.integer;
                    } else if (it.data_kind == .node_ref and it.node != null) {
                        const A = @TypeOf(ctx.adapter.*);
                        const ref = nodeRefFromRaw(A, it.node.?);
                        if (A.kind(ctx.adapter, ref) != .number) return error.InvalidOperand;
                        const text = A.numberText(ctx.adapter, ref);
                        const parsed = parseIntegerString(text) orelse return error.InvalidOperand;
                        total +%= parsed;
                    } else {
                        return error.InvalidOperand;
                    }
                }
                try out.append(ctx.allocator, makeIntegerItem(ctx, total));
                return out;
            },
            .decimal => {
                var max_scale: u32 = 0;
                for (input) |it| {
                    if (sumKindForItem(it, integer_type_id, decimal_type_id, quantity_type_id) != .decimal) {
                        return error.InvalidOperand;
                    }
                    const text = decimalTextFromItem(ctx, it) orelse return error.InvalidOperand;
                    const scale = decimalScale(text) orelse return error.InvalidOperand;
                    if (scale > max_scale) max_scale = scale;
                }

                var total: i128 = 0;
                for (input) |it| {
                    const text = decimalTextFromItem(ctx, it) orelse return error.InvalidOperand;
                    const scaled = parseDecimalScaled(text, max_scale) orelse return error.InvalidOperand;
                    const add = @addWithOverflow(total, scaled);
                    if (add[1] != 0) return error.InvalidOperand;
                    total = add[0];
                }

                const value_str = try formatScaledDecimal(ctx.allocator, total, max_scale);
                try out.append(ctx.allocator, makeDecimalItemText(ctx, value_str));
                return out;
            },
            .quantity => {
                var unit: ?[]const u8 = null;
                var max_scale: u32 = 0;
                for (input) |it| {
                    if (sumKindForItem(it, integer_type_id, decimal_type_id, quantity_type_id) != .quantity) {
                        return error.InvalidOperand;
                    }
                    if (it.data_kind != .value or it.value == null or it.value.? != .quantity) {
                        return error.InvalidOperand;
                    }
                    const q = it.value.?.quantity;
                    if (unit == null) {
                        unit = q.unit;
                    } else if (!std.mem.eql(u8, unit.?, q.unit)) {
                        return error.InvalidOperand;
                    }
                    const scale = decimalScale(q.value) orelse return error.InvalidOperand;
                    if (scale > max_scale) max_scale = scale;
                }

                var total: i128 = 0;
                for (input) |it| {
                    const q = it.value.?.quantity;
                    const scaled = parseDecimalScaled(q.value, max_scale) orelse return error.InvalidOperand;
                    const add = @addWithOverflow(total, scaled);
                    if (add[1] != 0) return error.InvalidOperand;
                    total = add[0];
                }

                const value_str = try formatScaledDecimal(ctx.allocator, total, max_scale);
                try out.append(ctx.allocator, makeQuantityItem(ctx, value_str, unit.?));
                return out;
            },
        }
    }
    if (std.mem.eql(u8, call.name, "min") or std.mem.eql(u8, call.name, "max")) {
        if (call.args.len != 0) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;

        const is_max = std.mem.eql(u8, call.name, "max");
        var best_idx: usize = 0;
        var best = input[0];

        for (input[1..], 1..) |candidate, idx| {
            const cmp = compareItems(ctx, candidate, best) orelse return error.InvalidOperand;
            if ((is_max and cmp > 0) or (!is_max and cmp < 0)) {
                best = candidate;
                best_idx = idx;
            }
        }

        var out = ItemList.empty;
        try out.append(ctx.allocator, input[best_idx]);
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
    if (std.mem.eql(u8, call.name, "precision")) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        const date_id = ctx.types.getOrAdd("System.Date") catch 0;
        const time_id = ctx.types.getOrAdd("System.Time") catch 0;
        const datetime_id = ctx.types.getOrAdd("System.DateTime") catch 0;

        if (val == .decimal) {
            const scale = decimalScale(val.decimal) orelse return out;
            try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(scale)));
            return out;
        }
        if (val == .date or it.type_id == date_id) {
            const text = if (val == .date) val.date else if (val == .string) val.string else return out;
            const prec = datePrecisionDigits(text) orelse return out;
            try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(prec)));
            return out;
        }
        if (val == .dateTime or it.type_id == datetime_id) {
            const text = if (val == .dateTime) val.dateTime else if (val == .string) val.string else return out;
            const prec = dateTimePrecisionDigits(text) orelse return out;
            try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(prec)));
            return out;
        }
        if (val == .time or it.type_id == time_id) {
            const text = if (val == .time) val.time else if (val == .string) val.string else return out;
            const prec = timePrecisionDigits(text) orelse return out;
            try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(prec)));
            return out;
        }
        return out;
    }
    if (std.mem.eql(u8, call.name, "lowBoundary") or std.mem.eql(u8, call.name, "highBoundary")) {
        if (call.args.len > 1) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;

        const it = input[0];
        const val = itemToValue(ctx, it);
        const kind: BoundaryKind = if (std.mem.eql(u8, call.name, "lowBoundary")) .low else .high;

        var precision_opt: ?i64 = null;
        if (call.args.len == 1) {
            precision_opt = try evalIntegerArg(ctx, call.args[0], env);
            if (precision_opt == null) return out;
        }
        if (precision_opt != null and precision_opt.? < 0) return out;

        const date_id = ctx.types.getOrAdd("System.Date") catch 0;
        const time_id = ctx.types.getOrAdd("System.Time") catch 0;
        const datetime_id = ctx.types.getOrAdd("System.DateTime") catch 0;
        const decimal_id = ctx.types.getOrAdd("System.Decimal") catch 0;
        const integer_id = ctx.types.getOrAdd("System.Integer") catch 0;

        if (val == .quantity) {
            const precision: u32 = if (precision_opt) |p| blk: {
                if (p > std.math.maxInt(u32)) return out;
                break :blk @intCast(p);
            } else DecimalMaxPrecision;
            if (precision > DecimalMaxPrecision) return out;

            const scale = decimalScale(val.quantity.value) orelse return out;
            const base_scaled = parseDecimalScaled(val.quantity.value, scale) orelse return out;
            const boundary_text = (try boundaryFromScaled(ctx.allocator, base_scaled, scale, precision, kind)) orelse return out;
            try out.append(ctx.allocator, makeQuantityItem(ctx, boundary_text, val.quantity.unit));
            return out;
        }

        if (val == .decimal or val == .integer or it.type_id == decimal_id or it.type_id == integer_id) {
            const precision: u32 = if (precision_opt) |p| blk: {
                if (p > std.math.maxInt(u32)) return out;
                break :blk @intCast(p);
            } else DecimalMaxPrecision;
            if (precision > DecimalMaxPrecision) return out;

            var scale: u32 = 0;
            var base_scaled: i128 = 0;
            if (val == .integer) {
                base_scaled = @intCast(val.integer);
                scale = 0;
            } else if (val == .decimal) {
                scale = decimalScale(val.decimal) orelse return out;
                base_scaled = parseDecimalScaled(val.decimal, scale) orelse return out;
            } else if (val == .string and (it.type_id == decimal_id or it.type_id == integer_id)) {
                scale = decimalScale(val.string) orelse return out;
                base_scaled = parseDecimalScaled(val.string, scale) orelse return out;
            } else {
                return out;
            }

            const boundary_text = (try boundaryFromScaled(ctx.allocator, base_scaled, scale, precision, kind)) orelse return out;
            try out.append(ctx.allocator, makeDecimalItemText(ctx, boundary_text));
            return out;
        }

        if (val == .date or it.type_id == date_id) {
            const precision: u32 = if (precision_opt) |p| blk: {
                if (p > std.math.maxInt(u32)) return out;
                break :blk @intCast(p);
            } else DateMaxPrecision;
            if (precision > DateMaxPrecision) return out;
            const text = if (val == .date) val.date else if (val == .string) val.string else return out;
            const boundary_text = (try buildDateBoundary(ctx.allocator, text, precision, kind)) orelse return out;
            try out.append(ctx.allocator, makeDateItem(ctx, boundary_text));
            return out;
        }

        if (val == .dateTime or it.type_id == datetime_id) {
            const precision: u32 = if (precision_opt) |p| blk: {
                if (p > std.math.maxInt(u32)) return out;
                break :blk @intCast(p);
            } else DateTimeMaxPrecision;
            if (precision > DateTimeMaxPrecision) return out;
            const text = if (val == .dateTime) val.dateTime else if (val == .string) val.string else return out;
            const boundary_text = (try buildDateTimeBoundary(ctx.allocator, text, precision, kind)) orelse return out;
            try out.append(ctx.allocator, makeDateTimeItem(ctx, boundary_text));
            return out;
        }

        if (val == .time or it.type_id == time_id) {
            const precision: u32 = if (precision_opt) |p| blk: {
                if (p > std.math.maxInt(u32)) return out;
                break :blk @intCast(p);
            } else TimeMaxPrecision;
            if (precision > TimeMaxPrecision) return out;
            const text = if (val == .time) val.time else if (val == .string) val.string else return out;
            const boundary_text = (try buildTimeBoundary(ctx.allocator, text, precision, kind)) orelse return out;
            try out.append(ctx.allocator, makeTimeItem(ctx, boundary_text));
            return out;
        }

        return out;
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
    if (std.mem.eql(u8, call.name, "subsetOf")) {
        if (call.args.len != 1) return error.InvalidFunction;
        var other = try evalCollectionArg(ctx, call.args[0], env);
        defer other.deinit(ctx.allocator);
        var is_subset = true;
        for (input) |it| {
            var found = false;
            for (other.items) |other_it| {
                if (itemsEqual(ctx, it, other_it)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                is_subset = false;
                break;
            }
        }
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, is_subset));
        return out;
    }
    if (std.mem.eql(u8, call.name, "supersetOf")) {
        if (call.args.len != 1) return error.InvalidFunction;
        var other = try evalCollectionArg(ctx, call.args[0], env);
        defer other.deinit(ctx.allocator);
        var is_superset = true;
        for (other.items) |other_it| {
            var found = false;
            for (input) |it| {
                if (itemsEqual(ctx, it, other_it)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                is_superset = false;
                break;
            }
        }
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, is_superset));
        return out;
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
    errdefer out.deinit(ctx.allocator);
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
        // Strip @ prefix from date/datetime literals (lexer captures @YYYY-MM-DD)
        .Date => |d| makeDateItem(ctx, if (d.len > 0 and d[0] == '@') d[1..] else d),
        .DateTime => |d| makeDateTimeItem(ctx, if (d.len > 0 and d[0] == '@') d[1..] else d),
        // Strip @T prefix from time literals (lexer captures @Thh:mm:ss)
        .Time => |t| makeTimeItem(ctx, if (t.len > 1 and t[0] == '@' and t[1] == 'T') t[2..] else t),
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

fn convertToInteger(ctx: anytype, it: item.Item) ?i64 {
    const val = itemToValue(ctx, it);
    switch (val) {
        .integer => |v| return v,
        .boolean => |v| return if (v) 1 else 0,
        .string => |s| return parseIntegerString(s),
        .decimal => |s| {
            const integer_type_id = ctx.types.getOrAdd("System.Integer") catch 0;
            if (it.type_id != integer_type_id) return null;
            return std.fmt.parseInt(i64, s, 10) catch null;
        },
        else => return null,
    }
}

fn convertToBoolean(ctx: anytype, it: item.Item) ?bool {
    const val = itemToValue(ctx, it);
    const integer_type_id = ctx.types.getOrAdd("System.Integer") catch 0;
    switch (val) {
        .boolean => |v| return v,
        .integer => |v| return if (v == 1) true else if (v == 0) false else null,
        .string => |s| return parseBooleanString(s),
        .decimal => |s| {
            if (it.type_id == integer_type_id) {
                const parsed = std.fmt.parseInt(i64, s, 10) catch return null;
                return if (parsed == 1) true else if (parsed == 0) false else null;
            }
            return parseBooleanDecimal(s);
        },
        else => return null,
    }
}

fn convertToDecimalText(ctx: anytype, it: item.Item) ?[]const u8 {
    const val = itemToValue(ctx, it);
    return switch (val) {
        .integer => |v| std.fmt.allocPrint(ctx.allocator, "{d}.0", .{v}) catch null,
        .decimal => |s| s,
        .boolean => |v| if (v) "1.0" else "0.0",
        .string => |s| if (isDecimalString(s)) s else null,
        else => null,
    };
}

fn canConvertToDecimal(ctx: anytype, it: item.Item) bool {
    const val = itemToValue(ctx, it);
    return switch (val) {
        .integer, .decimal, .boolean => true,
        .string => |s| isDecimalString(s),
        else => false,
    };
}

fn canConvertToString(ctx: anytype, it: item.Item) bool {
    const val = itemToValue(ctx, it);
    switch (val) {
        .boolean, .integer, .decimal, .string, .date, .time, .dateTime, .quantity => return true,
        else => {},
    }
    const quantity_type_id = ctx.types.getOrAdd("System.Quantity") catch 0;
    return it.type_id == quantity_type_id;
}

fn parseIntegerString(s: []const u8) ?i64 {
    if (s.len == 0) return null;
    var start: usize = 0;
    var sign: i64 = 1;
    if (s[0] == '+' or s[0] == '-') {
        sign = if (s[0] == '-') -1 else 1;
        start = 1;
        if (start == s.len) return null;
    }
    var i: usize = start;
    while (i < s.len) : (i += 1) {
        const c = s[i];
        if (c < '0' or c > '9') return null;
    }
    const magnitude = std.fmt.parseInt(i64, s[start..], 10) catch return null;
    return magnitude * sign;
}

fn isDecimalString(s: []const u8) bool {
    if (s.len == 0) return false;
    var idx: usize = 0;
    if (s[0] == '+' or s[0] == '-') {
        idx = 1;
        if (idx == s.len) return false;
    }
    var saw_dot = false;
    var digits_before: u32 = 0;
    var digits_after: u32 = 0;
    while (idx < s.len) : (idx += 1) {
        const c = s[idx];
        if (c >= '0' and c <= '9') {
            if (saw_dot) {
                digits_after += 1;
            } else {
                digits_before += 1;
            }
            continue;
        }
        if (c == '.') {
            if (saw_dot) return false;
            saw_dot = true;
            continue;
        }
        return false;
    }
    if (digits_before == 0) return false;
    if (saw_dot and digits_after == 0) return false;
    return true;
}

fn parseBooleanString(s: []const u8) ?bool {
    if (eqIgnoreCase(s, "true") or eqIgnoreCase(s, "t") or eqIgnoreCase(s, "yes") or eqIgnoreCase(s, "y") or eqIgnoreCase(s, "1") or eqIgnoreCase(s, "1.0")) {
        return true;
    }
    if (eqIgnoreCase(s, "false") or eqIgnoreCase(s, "f") or eqIgnoreCase(s, "no") or eqIgnoreCase(s, "n") or eqIgnoreCase(s, "0") or eqIgnoreCase(s, "0.0")) {
        return false;
    }
    return null;
}

fn parseBooleanDecimal(text: []const u8) ?bool {
    if (decimalEquivalentTo(text, 1)) return true;
    if (decimalEquivalentTo(text, 0)) return false;
    return null;
}

fn decimalEquivalentTo(text: []const u8, target: i128) bool {
    const scale = decimalScale(text) orelse return false;
    const scaled = parseDecimalScaled(text, scale) orelse return false;
    if (target == 0) return scaled == 0;
    var expected = target;
    var idx: u32 = 0;
    while (idx < scale) : (idx += 1) {
        const mul = @mulWithOverflow(expected, 10);
        if (mul[1] != 0) return false;
        expected = mul[0];
    }
    return scaled == expected;
}

fn eqIgnoreCase(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    var idx: usize = 0;
    while (idx < a.len) : (idx += 1) {
        if (std.ascii.toLower(a[idx]) != std.ascii.toLower(b[idx])) return false;
    }
    return true;
}

// Time string validation: hh[:mm[:ss[.fff]]] (partial times allowed)
// Per spec and official tests: '14', '14:34', '14:34:28', '14:34:28.123' are all valid
fn isValidTimeString(s: []const u8) bool {
    if (s.len == 0) return false;

    // Parse hour (2 digits required)
    if (s.len < 2) return false;
    if (!isDigit(s[0]) or !isDigit(s[1])) return false;
    const hour = (s[0] - '0') * 10 + (s[1] - '0');
    if (hour > 23) return false;

    // Hour-only partial time
    if (s.len == 2) return true;

    // Must have colon
    if (s[2] != ':') return false;
    if (s.len < 5) return false;
    if (!isDigit(s[3]) or !isDigit(s[4])) return false;
    const minute = (s[3] - '0') * 10 + (s[4] - '0');
    if (minute > 59) return false;

    // Hour:minute partial time
    if (s.len == 5) return true;

    // Must have colon
    if (s[5] != ':') return false;
    if (s.len < 8) return false;
    if (!isDigit(s[6]) or !isDigit(s[7])) return false;
    const second = (s[6] - '0') * 10 + (s[7] - '0');
    if (second > 59) return false;

    // Full seconds
    if (s.len == 8) return true;

    // Must have dot for fractional seconds
    if (s[8] != '.') return false;
    if (s.len < 10) return false;

    // Validate fractional part (1+ digits)
    var idx: usize = 9;
    while (idx < s.len) : (idx += 1) {
        if (!isDigit(s[idx])) return false;
    }
    return true;
}

// Date string validation: YYYY[-MM[-DD]] (partial dates allowed)
fn isValidDateString(s: []const u8) bool {
    if (s.len < 4) return false;

    // Parse year (4 digits)
    if (!isDigit(s[0]) or !isDigit(s[1]) or !isDigit(s[2]) or !isDigit(s[3])) return false;

    // Year-only partial date
    if (s.len == 4) return true;

    // Must have hyphen
    if (s[4] != '-') return false;
    if (s.len < 7) return false;
    if (!isDigit(s[5]) or !isDigit(s[6])) return false;
    const month = (s[5] - '0') * 10 + (s[6] - '0');
    if (month < 1 or month > 12) return false;

    // Year-month partial date
    if (s.len == 7) return true;

    // Must have hyphen
    if (s[7] != '-') return false;
    if (s.len < 10) return false;
    if (!isDigit(s[8]) or !isDigit(s[9])) return false;
    const day = (s[8] - '0') * 10 + (s[9] - '0');
    if (day < 1 or day > 31) return false;

    // Full date
    if (s.len == 10) return true;

    // Any extra characters are invalid for date-only
    return false;
}

// DateTime string validation: YYYY[-MM[-DD[Thh[:mm[:ss[.fff]]][timezone]]]]
fn isValidDateTimeString(s: []const u8) bool {
    if (s.len < 4) return false;

    // Parse year (4 digits)
    if (!isDigit(s[0]) or !isDigit(s[1]) or !isDigit(s[2]) or !isDigit(s[3])) return false;

    // Year-only partial datetime
    if (s.len == 4) return true;

    // Must have hyphen
    if (s[4] != '-') return false;
    if (s.len < 7) return false;
    if (!isDigit(s[5]) or !isDigit(s[6])) return false;
    const month = (s[5] - '0') * 10 + (s[6] - '0');
    if (month < 1 or month > 12) return false;

    // Year-month partial datetime
    if (s.len == 7) return true;

    // Must have hyphen
    if (s[7] != '-') return false;
    if (s.len < 10) return false;
    if (!isDigit(s[8]) or !isDigit(s[9])) return false;
    const day = (s[8] - '0') * 10 + (s[9] - '0');
    if (day < 1 or day > 31) return false;

    // Date-only partial datetime
    if (s.len == 10) return true;

    // Must have T for time part
    if (s[10] != 'T') return false;
    if (s.len < 13) return false;
    if (!isDigit(s[11]) or !isDigit(s[12])) return false;
    const hour = (s[11] - '0') * 10 + (s[12] - '0');
    if (hour > 23) return false;

    // DateTime with hour only
    if (s.len == 13) return true;

    // Check for timezone at this point (Z or +/-)
    if (s[13] == 'Z') return s.len == 14;
    if (s[13] == '+' or s[13] == '-') return isValidTimezone(s[13..]);

    // Must have colon for minutes
    if (s[13] != ':') return false;
    if (s.len < 16) return false;
    if (!isDigit(s[14]) or !isDigit(s[15])) return false;
    const minute = (s[14] - '0') * 10 + (s[15] - '0');
    if (minute > 59) return false;

    // DateTime with hour:minute
    if (s.len == 16) return true;

    // Check for timezone
    if (s[16] == 'Z') return s.len == 17;
    if (s[16] == '+' or s[16] == '-') return isValidTimezone(s[16..]);

    // Must have colon for seconds
    if (s[16] != ':') return false;
    if (s.len < 19) return false;
    if (!isDigit(s[17]) or !isDigit(s[18])) return false;
    const second = (s[17] - '0') * 10 + (s[18] - '0');
    if (second > 59) return false;

    // DateTime with hour:minute:second
    if (s.len == 19) return true;

    // Check for timezone
    if (s[19] == 'Z') return s.len == 20;
    if (s[19] == '+' or s[19] == '-') return isValidTimezone(s[19..]);

    // Must have dot for fractional seconds
    if (s[19] != '.') return false;
    if (s.len < 21) return false;

    // Validate fractional part (1+ digits)
    var idx: usize = 20;
    while (idx < s.len) : (idx += 1) {
        if (s[idx] == 'Z') return idx + 1 == s.len;
        if (s[idx] == '+' or s[idx] == '-') return isValidTimezone(s[idx..]);
        if (!isDigit(s[idx])) return false;
    }
    return true;
}

fn isValidTimezone(s: []const u8) bool {
    // Z
    if (s.len == 1 and s[0] == 'Z') return true;

    // +hh:mm or -hh:mm
    if (s.len != 6) return false;
    if (s[0] != '+' and s[0] != '-') return false;
    if (!isDigit(s[1]) or !isDigit(s[2])) return false;
    if (s[3] != ':') return false;
    if (!isDigit(s[4]) or !isDigit(s[5])) return false;
    const tz_hour = (s[1] - '0') * 10 + (s[2] - '0');
    const tz_minute = (s[4] - '0') * 10 + (s[5] - '0');
    if (tz_hour > 14) return false;
    if (tz_minute > 59) return false;
    return true;
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

// Convert item to time string (for toTime)
fn convertToTime(ctx: anytype, it: item.Item) ?[]const u8 {
    const val = itemToValue(ctx, it);
    switch (val) {
        .time => |t| return t, // passthrough
        .string => |s| {
            if (isValidTimeString(s)) return s;
            return null;
        },
        else => return null,
    }
}

// Convert item to date string (for toDate)
fn convertToDate(ctx: anytype, it: item.Item) ?[]const u8 {
    const val = itemToValue(ctx, it);
    switch (val) {
        .date => |d| return d, // passthrough
        .dateTime => |dt| {
            // Extract date part from datetime
            if (std.mem.indexOfScalar(u8, dt, 'T')) |t_pos| {
                return dt[0..t_pos];
            }
            // DateTime without T - just return as-is (date-only partial)
            return dt;
        },
        .string => |s| {
            if (isValidDateString(s)) return s;
            return null;
        },
        else => return null,
    }
}

// Convert item to datetime string (for toDateTime)
fn convertToDateTime(ctx: anytype, it: item.Item) ?[]const u8 {
    const val = itemToValue(ctx, it);
    switch (val) {
        .dateTime => |dt| return dt, // passthrough
        .date => |d| return d, // date promotes to datetime (time part empty)
        .string => |s| {
            if (isValidDateTimeString(s)) return s;
            return null;
        },
        else => return null,
    }
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

const SumKind = enum { integer, decimal, quantity };

fn sumKindForItem(
    it: item.Item,
    integer_type_id: u32,
    decimal_type_id: u32,
    quantity_type_id: u32,
) ?SumKind {
    if (it.data_kind == .value and it.value != null) {
        return switch (it.value.?) {
            .integer => .integer,
            .decimal => .decimal,
            .quantity => .quantity,
            else => null,
        };
    }
    if (it.data_kind == .node_ref) {
        if (it.type_id == integer_type_id) return .integer;
        if (it.type_id == decimal_type_id) return .decimal;
        if (it.type_id == quantity_type_id) return .quantity;
    }
    return null;
}

fn decimalTextFromItem(ctx: anytype, it: item.Item) ?[]const u8 {
    if (it.data_kind == .value and it.value != null and it.value.? == .decimal) {
        return it.value.?.decimal;
    }
    if (it.data_kind == .node_ref and it.node != null) {
        const A = @TypeOf(ctx.adapter.*);
        const ref = nodeRefFromRaw(A, it.node.?);
        if (A.kind(ctx.adapter, ref) != .number) return null;
        return A.numberText(ctx.adapter, ref);
    }
    return null;
}

fn decimalScale(text: []const u8) ?u32 {
    if (text.len == 0) return null;
    var idx: usize = 0;
    if (text[0] == '+' or text[0] == '-') {
        idx = 1;
        if (idx == text.len) return null;
    }
    var saw_dot = false;
    var saw_digit = false;
    var scale: u32 = 0;
    while (idx < text.len) : (idx += 1) {
        const c = text[idx];
        if (c >= '0' and c <= '9') {
            saw_digit = true;
            if (saw_dot) scale += 1;
            continue;
        }
        if (c == '.') {
            if (saw_dot) return null;
            saw_dot = true;
            continue;
        }
        if (c == 'e' or c == 'E') return null;
        return null;
    }
    if (!saw_digit) return null;
    return scale;
}

fn parseDecimalScaled(text: []const u8, target_scale: u32) ?i128 {
    if (text.len == 0) return null;
    var idx: usize = 0;
    var negative = false;
    if (text[0] == '+' or text[0] == '-') {
        negative = text[0] == '-';
        idx = 1;
        if (idx == text.len) return null;
    }

    var mag: i128 = 0;
    var scale: u32 = 0;
    var saw_dot = false;
    var saw_digit = false;

    while (idx < text.len) : (idx += 1) {
        const c = text[idx];
        if (c >= '0' and c <= '9') {
            saw_digit = true;
            const digit: i128 = @intCast(c - '0');
            const mul = @mulWithOverflow(mag, 10);
            if (mul[1] != 0) return null;
            const add = @addWithOverflow(mul[0], digit);
            if (add[1] != 0) return null;
            mag = add[0];
            if (saw_dot) scale += 1;
            continue;
        }
        if (c == '.') {
            if (saw_dot) return null;
            saw_dot = true;
            continue;
        }
        if (c == 'e' or c == 'E') return null;
        return null;
    }

    if (!saw_digit) return null;
    if (scale > target_scale) return null;

    var scaled = mag;
    var extra: u32 = target_scale - scale;
    while (extra > 0) : (extra -= 1) {
        const mul = @mulWithOverflow(scaled, 10);
        if (mul[1] != 0) return null;
        scaled = mul[0];
    }

    return if (negative) -scaled else scaled;
}

fn formatScaledDecimal(allocator: std.mem.Allocator, value: i128, scale: u32) ![]const u8 {
    const negative = value < 0;
    const abs_val: i128 = if (negative) -value else value;
    const digits = try std.fmt.allocPrint(allocator, "{d}", .{abs_val});
    defer allocator.free(digits);

    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(allocator);

    if (negative) try out.append(allocator, '-');

    if (scale == 0) {
        try out.appendSlice(allocator, digits);
        return try out.toOwnedSlice(allocator);
    }

    const digits_len = digits.len;
    const scale_usize: usize = @intCast(scale);
    if (digits_len <= scale_usize) {
        try out.append(allocator, '0');
        try out.append(allocator, '.');
        const zeros_needed = scale_usize - digits_len;
        var i: usize = 0;
        while (i < zeros_needed) : (i += 1) {
            try out.append(allocator, '0');
        }
        try out.appendSlice(allocator, digits);
        return try out.toOwnedSlice(allocator);
    }

    const point_pos = digits_len - scale_usize;
    try out.appendSlice(allocator, digits[0..point_pos]);
    try out.append(allocator, '.');
    try out.appendSlice(allocator, digits[point_pos..]);
    return try out.toOwnedSlice(allocator);
}

fn adjustScaledBoundary(value: i128, current_scale: u32, target_scale: u32, kind: BoundaryKind) ?i128 {
    if (current_scale == target_scale) return value;
    if (target_scale > current_scale) {
        const factor = pow10i128(target_scale - current_scale) orelse return null;
        const mul = @mulWithOverflow(value, factor);
        if (mul[1] != 0) return null;
        return mul[0];
    }

    const div = pow10i128(current_scale - target_scale) orelse return null;
    const q = @divTrunc(value, div);
    const r = @rem(value, div);
    if (r == 0) return q;

    const abs_r: i128 = if (r < 0) -r else r;
    const twice = @mulWithOverflow(abs_r, 2);
    if (twice[1] != 0) return q;

    if (twice[0] == div) {
        // Halfway case: round outward for the relevant boundary.
        if (kind == .high and value > 0) {
            const add = @addWithOverflow(q, 1);
            if (add[1] != 0) return null;
            return add[0];
        }
        if (kind == .low and value < 0) {
            const sub = @addWithOverflow(q, -1);
            if (sub[1] != 0) return null;
            return sub[0];
        }
    }

    return q;
}

fn boundaryFromScaled(
    allocator: std.mem.Allocator,
    base_scaled: i128,
    scale: u32,
    precision: u32,
    kind: BoundaryKind,
) EvalError!?[]const u8 {
    const uncertainty_scale: u32 = if (precision < scale) precision else scale;
    const work_scale: u32 = (if (scale > uncertainty_scale) scale else uncertainty_scale) + 1;

    // Scale value up to work_scale
    const scale_factor = pow10i128(work_scale - scale) orelse return null;
    const mul = @mulWithOverflow(base_scaled, scale_factor);
    if (mul[1] != 0) return null;

    const delta_factor = pow10i128(work_scale - uncertainty_scale - 1) orelse return null;
    const delta_mul = @mulWithOverflow(@as(i128, 5), delta_factor);
    if (delta_mul[1] != 0) return null;
    const delta: i128 = if (kind == .low) -delta_mul[0] else delta_mul[0];
    const boundary_add = @addWithOverflow(mul[0], delta);
    if (boundary_add[1] != 0) return null;

    const adjusted = adjustScaledBoundary(boundary_add[0], work_scale, precision, kind) orelse return null;
    if (adjusted == 0 and boundary_add[0] < 0) {
        // Preserve negative zero for boundary results that truncate to zero.
        if (precision == 0) return try allocator.dupe(u8, "-0");
        var out = std.ArrayList(u8).empty;
        errdefer out.deinit(allocator);
        try out.append(allocator, '-');
        try out.append(allocator, '0');
        try out.append(allocator, '.');
        var i: u32 = 0;
        while (i < precision) : (i += 1) {
            try out.append(allocator, '0');
        }
        return try out.toOwnedSlice(allocator);
    }
    return try formatScaledDecimal(allocator, adjusted, precision);
}

fn makeDecimalItemText(ctx: anytype, text: []const u8) item.Item {
    const type_id = ctx.types.getOrAdd("System.Decimal") catch 0;
    return .{
        .data_kind = .value,
        .value_kind = .decimal,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .decimal = text },
    };
}

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
