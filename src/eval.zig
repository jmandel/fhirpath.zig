const std = @import("std");
const ast = @import("ast.zig");
const node = @import("node.zig");
const item = @import("item.zig");
const schema = @import("schema.zig");
const regex = @import("regex.zig");
const JsonAdapter = @import("backends/json_adapter.zig").JsonAdapter;
const XmlAdapter = @import("backends/xml_adapter.zig").XmlAdapter;
const xml_parser = @import("backends/xml_parser.zig");

pub const ItemList = std.ArrayList(item.Item);

pub fn EvalContext(comptime A: type) type {
    return struct {
        allocator: std.mem.Allocator,
        adapter: *A,
        schema: ?*schema.Schema,
        root_item: item.Item = undefined,
        current_index: ?usize = null,
        timestamp: i64,
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

/// Adapter-independent evaluation result.
/// After eval, node_refs are resolved to std.json.Value entries so the
/// result can be read without any adapter reference. The arena is
/// transferred from the adapter/parser — no copies, just ownership transfer.
pub const EvalResult = struct {
    items: ItemList,
    /// Resolved std.json.Value for each node_ref item.
    /// item.node.? is an index into this array when data_kind == .node_ref.
    node_values: std.ArrayListUnmanaged(std.json.Value),
    /// Owns all allocated memory (parsed input, eval items, resolved values).
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: *EvalResult) void {
        self.arena.deinit();
    }

    /// Get the resolved std.json.Value for a node_ref item.
    pub fn nodeValue(self: *const EvalResult, it: item.Item) ?std.json.Value {
        const idx = it.node orelse return null;
        if (idx >= self.node_values.items.len) return null;
        return self.node_values.items[idx];
    }
};

pub fn evalWithJson(
    allocator: std.mem.Allocator,
    expr: ast.Expr,
    json_text: []const u8,
    env: ?*Env,
    schema_ptr: ?*schema.Schema,
) !EvalResult {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const arena_alloc = arena.allocator();
    const root_val = try arena_alloc.create(std.json.Value);
    root_val.* = try std.json.parseFromSliceLeaky(std.json.Value, arena_alloc, json_text, .{ .parse_numbers = false });
    const flavor: JsonAdapter.Flavor = if (schema_ptr != null) .fhir_json else .generic_json;
    var adapter = JsonAdapter.init(arena_alloc, root_val, flavor);
    var ctx = EvalContext(JsonAdapter){
        .allocator = arena_alloc,
        .adapter = &adapter,
        .schema = schema_ptr,
        .timestamp = std.time.timestamp(),
    };
    const items = try evalExpression(&ctx, expr, adapter.root(), env);
    return resolveResult(JsonAdapter, &adapter, items, &arena, schema_ptr);
}

pub fn evalWithXml(
    allocator: std.mem.Allocator,
    expr: ast.Expr,
    xml_text: []const u8,
    env: ?*Env,
    schema_ptr: ?*schema.Schema,
) !EvalResult {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const arena_alloc = arena.allocator();
    const root_node = xml_parser.parse(arena_alloc, xml_text) catch return error.InvalidJson;
    var adapter = XmlAdapter.init(arena_alloc, root_node);
    var ctx = EvalContext(XmlAdapter){
        .allocator = arena_alloc,
        .adapter = &adapter,
        .schema = schema_ptr,
        .timestamp = std.time.timestamp(),
    };
    const items = try evalExpression(&ctx, expr, adapter.root(), env);
    return resolveResult(XmlAdapter, &adapter, items, &arena, schema_ptr);
}

/// Resolve adapter-specific node_ref items to adapter-independent std.json.Value entries,
/// then take ownership of the arena. All data (parsed input, eval items, resolved values)
/// already lives in this arena — no copies, just ownership transfer.
///
/// After this returns, the adapter struct can go out of scope (its heap data is in the arena).
pub fn resolveResult(
    comptime A: type,
    adapter: *A,
    items: ItemList,
    arena: *std.heap.ArenaAllocator,
    schema_ptr: ?*schema.Schema,
) !EvalResult {
    const convert = @import("convert.zig");
    const value_resolver = @import("value_resolver.zig");
    const arena_alloc = arena.allocator();
    var node_values = std.ArrayListUnmanaged(std.json.Value){};

    for (items.items) |*it| {
        if (it.data_kind == .node_ref and it.node != null) {
            const ref = it.node.?;
            const jv = try convert.adapterNodeToJsonValue(A, arena_alloc, adapter, ref);
            const idx = node_values.items.len;
            try node_values.append(arena_alloc, jv);
            // Promote primitive node_refs to value items using centralized resolver
            const val = value_resolver.nodeToValue(A, adapter, ref, it.type_id, schema_ptr);
            if (val != .empty) {
                it.data_kind = .value;
                it.value_kind = std.meta.activeTag(val);
                it.value = val;
            }
            // Always update node to resolved index (for objects/arrays that stay as node_ref)
            it.node = idx;
        }
        // Serialize value items (TypeInfo, Quantity, etc.) into node_values
        // so the WASM layer can read them via the generic node JSON path.
        if (it.data_kind == .value and it.value != null and it.node == null) {
            const jv = convert.valueToJsonValue(arena_alloc, it.value.?) catch null;
            if (jv) |v| {
                const idx = node_values.items.len;
                try node_values.append(arena_alloc, v);
                it.node = idx;
            }
        }
    }
    return .{
        .items = items,
        .node_values = node_values,
        .arena = arena.*,
    };
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

// NodeRef = NodeHandle = usize, same as Item.node storage type.
// No conversion needed — use directly.

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
    // Check if any step uses defineVariable - if so, we need a local env
    const needs_local_env = for (path.steps) |step| {
        switch (step) {
            .Function => |call| {
                if (std.mem.eql(u8, call.name, "defineVariable")) break true;
            },
            else => {},
        }
    } else false;

    // Create local env if needed (for defineVariable support)
    var local_env: ?Env = null;
    defer if (local_env) |*le| le.deinit();

    var effective_env: ?*Env = env;
    if (needs_local_env) {
        local_env = Env.init(ctx.allocator);
        // Copy parent env if it exists
        if (env) |e| {
            var it = e.map.iterator();
            while (it.next()) |entry| {
                try local_env.?.map.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }
        effective_env = &local_env.?;
    }

    var current = ItemList.empty;
    errdefer current.deinit(ctx.allocator);

    switch (path.root) {
        .This => try current.append(ctx.allocator, root_item),
        .Env => |name| {
            if (effective_env) |e| {
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
            if (effective_env) |e| {
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
                const next = try evalFunction(ctx, call, current.items, effective_env);
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
    // Handle TypeInfo property access
    if (it.data_kind == .value and it.value_kind == .typeInfo and it.value != null) {
        const ti = it.value.?.typeInfo;
        if (std.mem.eql(u8, name, "namespace")) {
            try out.append(ctx.allocator, makeStringItem(ctx, ti.namespace));
            return;
        }
        if (std.mem.eql(u8, name, "name")) {
            try out.append(ctx.allocator, makeStringItem(ctx, ti.name));
            return;
        }
        // Unknown property on TypeInfo - return empty
        return;
    }

    if (it.node == null) return;
    const A = @TypeOf(ctx.adapter.*);
    const ref = it.node.?;
    var child_type_id: u32 = 0;
    if (ctx.schema) |s| {
        if (schema.isModelType(it.type_id)) {
            if (s.childTypeForField(it.type_id, name)) |tid| child_type_id = tid;
        }
    }
    switch (A.kind(ctx.adapter, ref)) {
        .object => {
            // First try to get a child property with the given name
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
            } else {
                // Property not found - check if this is a choice type field in the schema.
                // For choice types (e.g., value[x] in FHIR), we need to check for concrete variants
                // like valueQuantity, valueString, etc.
                var found_choice = false;
                if (ctx.schema) |s| {
                    if (schema.isModelType(it.type_id)) {
                        if (s.choiceGroupForField(it.type_id, name)) |group_id| {
                            // This is a choice type - look for any variant in the JSON
                            var variants = s.choiceVariantsForType(it.type_id, group_id);
                            while (variants.next()) |v| {
                                if (A.objectGet(ctx.adapter, ref, v.name)) |variant_ref| {
                                    found_choice = true;
                                    if (A.kind(ctx.adapter, variant_ref) == .array) {
                                        const len = A.arrayLen(ctx.adapter, variant_ref);
                                        for (0..len) |i| {
                                            const variant_item_ref = A.arrayAt(ctx.adapter, variant_ref, i);
                                            const child = try itemFromNodeWithType(ctx, variant_item_ref, v.child_type_id);
                                            try out.append(ctx.allocator, child);
                                        }
                                    } else {
                                        const child = try itemFromNodeWithType(ctx, variant_ref, v.child_type_id);
                                        try out.append(ctx.allocator, child);
                                    }
                                    break; // Only one variant should be present
                                }
                            }
                        }
                    }
                }

                if (!found_choice) {
                    // Property not found - check if name is a type that matches this item's type.
                    // Per FHIRPath spec: "When resolving an identifier that is also the root of a
                    // FHIRPath expression, it is resolved as a type name first, and if it resolves
                    // to a type, it must resolve to the type of the context (or a supertype)."
                    // This applies to the first identifier in a path like "Patient.active".
                    if (itemMatchesType(ctx, it, name)) {
                        try out.append(ctx.allocator, it);
                    }
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
        .string, .number, .bool, .null => {
            // FHIR merged primitives may support objectGet even though kind is
            // primitive. For non-FHIR adapters, objectGet returns null for
            // non-object nodes, so this is safe.
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
        const eq = itemsEqualTriState(ctx, left[0], right[0]);
        if (eq) |e| {
            try out.append(ctx.allocator, makeBoolItem(ctx, e));
        }
        // eq == null means empty (precision mismatch)
        return out;
    }
    // For collections: are they the same?
    if (left.len != right.len) {
        try out.append(ctx.allocator, makeBoolItem(ctx, false));
        return out;
    }
    // Check each item pairwise. Per spec:
    // - any false -> false; all true -> true; any empty (and no false) -> empty
    var has_empty = false;
    for (left, 0..) |l, i| {
        const eq = itemsEqualTriState(ctx, l, right[i]);
        if (eq) |e| {
            if (!e) {
                try out.append(ctx.allocator, makeBoolItem(ctx, false));
                return out;
            }
        } else {
            has_empty = true;
        }
    }
    if (has_empty) return out; // empty
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
    // Normalize to UTC for comparison (respecting timezone offsets per spec)
    const a_norm = normalizeToUtc(a) orelse return false;
    const b_norm = normalizeToUtc(b) orelse return false;

    // For equivalence, if precisions differ return false (not empty like equality)
    if (a_norm.date_prec != b_norm.date_prec) return false;
    if (a_norm.has_time != b_norm.has_time) return false;
    if (a_norm.has_time and a_norm.time_prec != b_norm.time_prec) return false;

    // Both must have timezone or both must not have timezone
    if (a_norm.has_time and a_norm.has_tz != b_norm.has_tz) return false;

    // Compare normalized values
    if (a_norm.year != b_norm.year) return false;
    if (a_norm.date_prec >= 1 and a_norm.month != b_norm.month) return false;
    if (a_norm.date_prec >= 2 and a_norm.day != b_norm.day) return false;

    if (a_norm.has_time) {
        if (a_norm.time_prec >= 1 and a_norm.hour != b_norm.hour) return false;
        if (a_norm.time_prec >= 2 and a_norm.minute != b_norm.minute) return false;
        if (a_norm.time_prec >= 3) {
            if (a_norm.second != b_norm.second) return false;
            if (a_norm.millis != b_norm.millis) return false;
        }
    }

    return true;
}

fn valueEquivalent(a_val: item.Value, b_val: item.Value) bool {
    if (a_val == .empty or b_val == .empty) return a_val == .empty and b_val == .empty;

    if (a_val == .date or b_val == .date) {
        if (!(a_val == .date and b_val == .date)) return false;
        return dateEquivalent(a_val.date, b_val.date);
    }

    if (a_val == .time or b_val == .time) {
        if (!(a_val == .time and b_val == .time)) return false;
        return timeEquivalent(a_val.time, b_val.time);
    }

    if (a_val == .dateTime or b_val == .dateTime) {
        if (!(a_val == .dateTime and b_val == .dateTime)) return false;
        return dateTimeEquivalent(a_val.dateTime, b_val.dateTime);
    }

    const a_numeric = a_val == .integer or a_val == .long or a_val == .decimal;
    const b_numeric = b_val == .integer or b_val == .long or b_val == .decimal;
    if (a_numeric and b_numeric) {
        var buf_a: [64]u8 = undefined;
        var buf_b: [64]u8 = undefined;
        const text_a = switch (a_val) {
            .integer => std.fmt.bufPrint(&buf_a, "{d}", .{a_val.integer}) catch return false,
            .long => std.fmt.bufPrint(&buf_a, "{d}", .{a_val.long}) catch return false,
            .decimal => a_val.decimal,
            else => return false,
        };
        const text_b = switch (b_val) {
            .integer => std.fmt.bufPrint(&buf_b, "{d}", .{b_val.integer}) catch return false,
            .long => std.fmt.bufPrint(&buf_b, "{d}", .{b_val.long}) catch return false,
            .decimal => b_val.decimal,
            else => return false,
        };
        return decimalEquivalentText(text_a, text_b);
    }

    if (a_val == .string and b_val == .string) {
        return stringEquivalent(a_val.string, b_val.string);
    }

    if (a_val == .boolean and b_val == .boolean) return a_val.boolean == b_val.boolean;
    if (a_val == .quantity and b_val == .quantity) {
        // For equivalence, use the equivalence check which includes year/month mappings
        if (!quantityUnitsEquivalent(a_val.quantity.unit, b_val.quantity.unit)) return false;
        // Use numeric comparison for quantity values (not string comparison)
        const fa = std.fmt.parseFloat(f64, a_val.quantity.value) catch return false;
        const fb = std.fmt.parseFloat(f64, b_val.quantity.value) catch return false;
        return fa == fb;
    }

    return false;
}

fn itemsEquivalent(ctx: anytype, a: item.Item, b: item.Item) bool {
    const A = @TypeOf(ctx.adapter.*);
    const a_is_node = a.data_kind == .node_ref and a.node != null;
    const b_is_node = b.data_kind == .node_ref and b.node != null;

    if (a_is_node and b_is_node) {
        const a_ref = a.node.?;
        const b_ref = b.node.?;
        const kind_a = A.kind(ctx.adapter, a_ref);
        const kind_b = A.kind(ctx.adapter, b_ref);
        if (kind_a == .array or kind_a == .object or kind_b == .array or kind_b == .object) {
            if (kind_a != kind_b) return false;
            return nodeEqual(ctx, a_ref, b_ref);
        }
        // For scalar nodes, resolve to values for proper type tags
        return valueEquivalent(itemToValue(ctx, a), itemToValue(ctx, b));
    }

    if (a_is_node or b_is_node) {
        if (a_is_node) {
            const a_ref = a.node.?;
            const kind_a = A.kind(ctx.adapter, a_ref);
            if (kind_a == .array or kind_a == .object) return false;
        }
        if (b_is_node) {
            const b_ref = b.node.?;
            const kind_b = A.kind(ctx.adapter, b_ref);
            if (kind_b == .array or kind_b == .object) return false;
        }
    }

    return valueEquivalent(itemToValue(ctx, a), itemToValue(ctx, b));
}

const CompareOp = enum { lt, le, gt, ge };

fn evalComparison(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
    op: CompareOp,
) EvalError!ItemList {
    var out = ItemList.empty;
    // Empty propagates
    if (left.len == 0 or right.len == 0) return out;
    // Multi-item operands are errors per spec
    if (left.len != 1 or right.len != 1) return error.SingletonRequired;

    const l = left[0];
    const r = right[0];

    const cmp = compareItems(ctx, l, r) catch |err| return err;
    if (cmp) |c| {
        const result = switch (op) {
            .lt => c < 0,
            .le => c <= 0,
            .gt => c > 0,
            .ge => c >= 0,
        };
        try out.append(ctx.allocator, makeBoolItem(ctx, result));
    }
    // cmp == null means precision mismatch -> return empty
    return out;
}

fn compareItems(ctx: anytype, a: item.Item, b: item.Item) EvalError!?i32 {
    const va = itemToValue(ctx, a);
    const vb = itemToValue(ctx, b);

    // Check if either operand is Date or DateTime (by value kind)
    const a_is_date = va == .date;
    const a_is_datetime = va == .dateTime;
    const b_is_date = vb == .date;
    const b_is_datetime = vb == .dateTime;

    // Cross-type Date/DateTime comparison (implicit conversion)
    if ((a_is_date or a_is_datetime) and (b_is_date or b_is_datetime)) {
        const a_text = if (va == .dateTime) va.dateTime else va.date;
        const b_text = if (vb == .dateTime) vb.dateTime else vb.date;
        return compareDateAndDateTime(a_text, a_is_datetime, b_text, b_is_datetime);
    }

    // Compare integers (including Long)
    if ((va == .integer or va == .long) and (vb == .integer or vb == .long)) {
        const ai: i64 = switch (va) {
            .integer => va.integer,
            .long => va.long,
            else => unreachable,
        };
        const bi: i64 = switch (vb) {
            .integer => vb.integer,
            .long => vb.long,
            else => unreachable,
        };
        if (ai < bi) return -1;
        if (ai > bi) return 1;
        return 0;
    }

    // Compare decimals or mixed int/long/decimal
    const fa: ?f64 = switch (va) {
        .integer => |i| @floatFromInt(i),
        .long => |i| @floatFromInt(i),
        .decimal => |d| std.fmt.parseFloat(f64, d) catch null,
        else => null,
    };
    const fb: ?f64 = switch (vb) {
        .integer => |i| @floatFromInt(i),
        .long => |i| @floatFromInt(i),
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

    // Compare times - precision mismatch yields empty
    if (va == .time and vb == .time) {
        const a_prec = timePrecisionLevel(va.time);
        const b_prec = timePrecisionLevel(vb.time);
        if (a_prec != b_prec) return null; // different precision
        return compareTimeStrings(va.time, vb.time);
    }

    // Compare quantities (with calendar duration unit conversion)
    if (va == .quantity and vb == .quantity) {
        return compareQuantitiesOrdered(va.quantity, vb.quantity);
    }

    // Non-convertible types are errors for comparison
    return error.InvalidOperand;
}

/// Compare time strings handling seconds+milliseconds as decimal.
/// Both strings must have same precision level (checked by caller).
fn compareTimeStrings(a: []const u8, b: []const u8) i32 {
    // Find the seconds part (after second colon) and compare the rest lexicographically
    const a_sec_pos = secondColonPos(a);
    const b_sec_pos = secondColonPos(b);

    if (a_sec_pos != null and b_sec_pos != null) {
        // Compare everything before seconds
        const prefix_cmp = std.mem.order(u8, a[0..a_sec_pos.?], b[0..b_sec_pos.?]);
        if (prefix_cmp != .eq) return switch (prefix_cmp) {
            .lt => @as(i32, -1),
            .gt => @as(i32, 1),
            .eq => unreachable,
        };
        // Compare seconds as decimal (trailing zeros don't matter)
        const a_sec = a[a_sec_pos.? + 1 ..];
        const b_sec = b[b_sec_pos.? + 1 ..];
        return compareDecimalStrings(a_sec, b_sec);
    }
    // No seconds part - simple lexicographic compare
    return switch (std.mem.order(u8, a, b)) {
        .lt => -1,
        .eq => 0,
        .gt => 1,
    };
}

/// Compare dateTime strings handling seconds+milliseconds as decimal.
/// Compare Date and DateTime values with implicit conversion.
/// When comparing Date vs DateTime:
///   - Date is implicitly converted to DateTime with empty time components
///   - The comparison proceeds precision by precision
///   - If one value has a precision the other doesn't, result is empty (null)
///   - If values differ at any precision, comparison stops and returns result
fn compareDateAndDateTime(a: []const u8, a_is_datetime: bool, b: []const u8, b_is_datetime: bool) ?i32 {
    // Use normalized comparison that handles timezone offsets correctly
    const a_norm = normalizeToUtc(a) orelse return null;
    const b_norm = normalizeToUtc(b) orelse return null;

    // Adjust has_time based on whether this is actually a DateTime type
    // A Date type never has time; a DateTime may or may not have time
    var a_adj = a_norm;
    var b_adj = b_norm;
    if (!a_is_datetime) a_adj.has_time = false;
    if (!b_is_datetime) b_adj.has_time = false;

    return compareNormalized(a_adj, b_adj);
}

/// Count date precision by number of hyphens (0=year, 1=month, 2=day)
fn countDatePrecision(date: []const u8) u8 {
    var hyphens: u8 = 0;
    for (date) |c| {
        if (c == '-' and hyphens < 2) hyphens += 1;
    }
    return hyphens;
}

/// Compare date portions at specified precision level
fn compareDatePortionAtPrecision(a: []const u8, b: []const u8, prec: u8) i32 {
    // Get the substring up to the precision level
    const a_end = dateEndAtPrecision(a, prec);
    const b_end = dateEndAtPrecision(b, prec);
    return switch (std.mem.order(u8, a[0..a_end], b[0..b_end])) {
        .lt => -1,
        .eq => 0,
        .gt => 1,
    };
}

/// Get end index for date string at given precision
fn dateEndAtPrecision(date: []const u8, prec: u8) usize {
    var hyphens: u8 = 0;
    for (date, 0..) |c, i| {
        if (c == '-') {
            hyphens += 1;
            if (hyphens > prec) return i;
        }
    }
    return date.len;
}

fn compareDateTimeStrings(a: []const u8, b: []const u8) i32 {
    // Find T separator
    const a_t = std.mem.indexOfScalar(u8, a, 'T');
    const b_t = std.mem.indexOfScalar(u8, b, 'T');

    if (a_t != null and b_t != null) {
        // Compare date part
        const date_cmp = std.mem.order(u8, a[0..a_t.?], b[0..b_t.?]);
        if (date_cmp != .eq) return switch (date_cmp) {
            .lt => @as(i32, -1),
            .gt => @as(i32, 1),
            .eq => unreachable,
        };
        // Compare time part (stripping timezone for comparison)
        const a_time = stripTimezone(a[a_t.? + 1 ..]);
        const b_time = stripTimezone(b[b_t.? + 1 ..]);
        return compareTimeStrings(a_time, b_time);
    }
    // No time component - simple lexicographic
    return switch (std.mem.order(u8, a, b)) {
        .lt => -1,
        .eq => 0,
        .gt => 1,
    };
}

/// Find position of second colon in a time string (the one before seconds).
fn secondColonPos(t: []const u8) ?usize {
    var count: u8 = 0;
    for (t, 0..) |c, i| {
        if (c == ':') {
            count += 1;
            if (count == 2) return i;
        }
    }
    return null;
}

/// Strip timezone suffix from a time string.
fn stripTimezone(t: []const u8) []const u8 {
    if (t.len > 0 and t[t.len - 1] == 'Z') return t[0 .. t.len - 1];
    if (t.len >= 6 and (t[t.len - 6] == '+' or t[t.len - 6] == '-')) return t[0 .. t.len - 6];
    return t;
}

/// Timezone offset info: offset in minutes and whether present
const TimezoneInfo = struct {
    has_tz: bool,
    offset_mins: i32, // minutes from UTC (positive = east, negative = west)
};

/// Extract timezone offset from a time string.
/// Returns (has_tz, offset_minutes) where offset is minutes from UTC.
/// Z means +00:00. +HH:MM is positive, -HH:MM is negative.
fn extractTimezone(t: []const u8) TimezoneInfo {
    if (t.len == 0) return .{ .has_tz = false, .offset_mins = 0 };

    // Check for Z suffix
    if (t[t.len - 1] == 'Z') return .{ .has_tz = true, .offset_mins = 0 };

    // Check for +/-HH:MM at end (6 chars)
    if (t.len >= 6) {
        const sign_pos = t.len - 6;
        const sign = t[sign_pos];
        if (sign == '+' or sign == '-') {
            // Parse HH:MM
            const hh = std.fmt.parseInt(i32, t[sign_pos + 1 .. sign_pos + 3], 10) catch return .{ .has_tz = false, .offset_mins = 0 };
            const mm = std.fmt.parseInt(i32, t[sign_pos + 4 .. sign_pos + 6], 10) catch return .{ .has_tz = false, .offset_mins = 0 };
            var offset = hh * 60 + mm;
            if (sign == '-') offset = -offset;
            return .{ .has_tz = true, .offset_mins = offset };
        }
    }

    return .{ .has_tz = false, .offset_mins = 0 };
}

/// Convert a datetime string to UTC-normalized components for comparison.
/// Returns null if the datetime cannot be parsed.
const NormalizedDateTime = struct {
    // Date components
    year: i32,
    month: i32, // 1-12 or 0 if not specified
    day: i32, // 1-31 or 0 if not specified
    // Time components (only valid if has_time)
    has_time: bool,
    hour: i32, // 0-23
    minute: i32, // 0-59
    second: i32, // 0-59
    millis: i32, // 0-999
    // Precision info
    date_prec: u8, // 0=year, 1=month, 2=day
    time_prec: u8, // 1=hour, 2=minute, 3=seconds (includes millis)
    // Timezone
    has_tz: bool,
};

fn normalizeToUtc(dt: []const u8) ?NormalizedDateTime {
    var result: NormalizedDateTime = .{
        .year = 0,
        .month = 0,
        .day = 0,
        .has_time = false,
        .hour = 0,
        .minute = 0,
        .second = 0,
        .millis = 0,
        .date_prec = 0,
        .time_prec = 0,
        .has_tz = false,
    };

    // Find T separator
    const t_pos = std.mem.indexOfScalar(u8, dt, 'T');
    const date_part = dt[0 .. t_pos orelse dt.len];

    // Parse date: YYYY[-MM[-DD]]
    result.year = std.fmt.parseInt(i32, date_part[0..4], 10) catch return null;
    result.date_prec = 0; // year

    if (date_part.len >= 7 and date_part[4] == '-') {
        result.month = std.fmt.parseInt(i32, date_part[5..7], 10) catch return null;
        result.date_prec = 1; // month

        if (date_part.len >= 10 and date_part[7] == '-') {
            result.day = std.fmt.parseInt(i32, date_part[8..10], 10) catch return null;
            result.date_prec = 2; // day
        }
    }

    // Parse time if present
    if (t_pos) |tp| {
        if (tp + 1 >= dt.len) return result; // No time after T

        const time_full = dt[tp + 1 ..];
        const tz_info = extractTimezone(time_full);
        result.has_tz = tz_info.has_tz;

        const time_part = stripTimezone(time_full);
        if (time_part.len == 0) return result;

        result.has_time = true;

        // Parse time: HH[:MM[:SS[.fff]]]
        if (time_part.len >= 2) {
            result.hour = std.fmt.parseInt(i32, time_part[0..2], 10) catch return null;
            result.time_prec = 1; // hour
        }

        if (time_part.len >= 5 and time_part[2] == ':') {
            result.minute = std.fmt.parseInt(i32, time_part[3..5], 10) catch return null;
            result.time_prec = 2; // minute
        }

        if (time_part.len >= 8 and time_part[5] == ':') {
            result.second = std.fmt.parseInt(i32, time_part[6..8], 10) catch return null;
            result.time_prec = 3; // seconds

            // Parse milliseconds
            if (time_part.len > 8 and time_part[8] == '.') {
                // Get digits after decimal
                var end: usize = 9;
                while (end < time_part.len and time_part[end] >= '0' and time_part[end] <= '9') {
                    end += 1;
                }
                if (end > 9) {
                    // Pad or truncate to 3 digits
                    var frac_str: [3]u8 = .{ '0', '0', '0' };
                    const frac_len = @min(end - 9, 3);
                    for (0..frac_len) |i| {
                        frac_str[i] = time_part[9 + i];
                    }
                    result.millis = std.fmt.parseInt(i32, &frac_str, 10) catch 0;
                }
            }
        }

        // Apply timezone offset to normalize to UTC
        if (tz_info.has_tz and tz_info.offset_mins != 0) {
            // Subtract offset to get UTC
            // E.g., 15:00+02:00 -> 13:00 UTC
            result.minute -= tz_info.offset_mins;

            // Handle minute overflow/underflow
            while (result.minute < 0) {
                result.minute += 60;
                result.hour -= 1;
            }
            while (result.minute >= 60) {
                result.minute -= 60;
                result.hour += 1;
            }

            // Handle hour overflow/underflow
            while (result.hour < 0) {
                result.hour += 24;
                result.day -= 1;
            }
            while (result.hour >= 24) {
                result.hour -= 24;
                result.day += 1;
            }

            // Handle day overflow/underflow (simplified - use 30-day months for comparison)
            if (result.day <= 0) {
                result.month -= 1;
                if (result.month <= 0) {
                    result.month = 12;
                    result.year -= 1;
                }
                result.day += daysInMonthNorm(result.year, result.month);
            }
            const max_day = daysInMonthNorm(result.year, result.month);
            if (result.day > max_day) {
                result.day -= max_day;
                result.month += 1;
                if (result.month > 12) {
                    result.month = 1;
                    result.year += 1;
                }
            }
        }
    }

    return result;
}

fn daysInMonthNorm(year: i32, month: i32) i32 {
    // Use the existing daysInMonth but convert types
    if (month < 1 or month > 12) return 30; // fallback
    return @intCast(daysInMonth(year, @intCast(@as(u32, @intCast(month)))) orelse 30);
}

fn compareNormalized(a: NormalizedDateTime, b: NormalizedDateTime) ?i32 {
    // Compare date components at shared precision
    const shared_date_prec = @min(a.date_prec, b.date_prec);

    // Year
    if (a.year < b.year) return -1;
    if (a.year > b.year) return 1;

    // Month (if both have month precision)
    if (shared_date_prec >= 1) {
        if (a.month < b.month) return -1;
        if (a.month > b.month) return 1;
    }

    // Day (if both have day precision)
    if (shared_date_prec >= 2) {
        if (a.day < b.day) return -1;
        if (a.day > b.day) return 1;
    }

    // Date portions equal at shared precision
    if (a.date_prec != b.date_prec) return null; // different date precision

    // Check time presence
    if (!a.has_time and !b.has_time) return 0; // both date-only
    if (a.has_time != b.has_time) return null; // one has time, other doesn't

    // Both have time - check timezone compatibility
    // If one has TZ and other doesn't, result is unknown
    if (a.has_tz != b.has_tz) return null;

    // Compare time at shared precision
    const shared_time_prec = @min(a.time_prec, b.time_prec);

    // Hour
    if (shared_time_prec >= 1) {
        if (a.hour < b.hour) return -1;
        if (a.hour > b.hour) return 1;
    }

    // Minute
    if (shared_time_prec >= 2) {
        if (a.minute < b.minute) return -1;
        if (a.minute > b.minute) return 1;
    }

    // Seconds+millis (combined precision)
    if (shared_time_prec >= 3) {
        if (a.second < b.second) return -1;
        if (a.second > b.second) return 1;
        if (a.millis < b.millis) return -1;
        if (a.millis > b.millis) return 1;
    }

    // Time portions equal at shared precision
    if (a.time_prec != b.time_prec) return null;

    return 0;
}

/// Compare two decimal number strings, treating trailing zeros as equal.
/// E.g., "00" == "00.0", "31.0" == "31", "31.1" > "31"
fn compareDecimalStrings(a: []const u8, b: []const u8) i32 {
    // Parse into integer part and fractional part
    const a_dot = std.mem.indexOfScalar(u8, a, '.');
    const b_dot = std.mem.indexOfScalar(u8, b, '.');

    const a_int = a[0 .. a_dot orelse a.len];
    const b_int = b[0 .. b_dot orelse b.len];

    // Compare integer parts
    const int_cmp = std.mem.order(u8, a_int, b_int);
    if (int_cmp != .eq) return switch (int_cmp) {
        .lt => -1,
        .gt => 1,
        .eq => unreachable,
    };

    // Compare fractional parts (missing = "")
    const a_frac = if (a_dot) |d| a[d + 1 ..] else "";
    const b_frac = if (b_dot) |d| b[d + 1 ..] else "";

    // Pad shorter with zeros conceptually
    const max_len = @max(a_frac.len, b_frac.len);
    var i: usize = 0;
    while (i < max_len) : (i += 1) {
        const ac: u8 = if (i < a_frac.len) a_frac[i] else '0';
        const bc: u8 = if (i < b_frac.len) b_frac[i] else '0';
        if (ac < bc) return -1;
        if (ac > bc) return 1;
    }
    return 0;
}

/// Returns a precision level for time strings (hours=1, minutes=2, seconds=3)
/// Per spec, seconds and milliseconds are considered a single precision.
fn timePrecisionLevel(t: []const u8) u8 {
    // Time format: HH[:MM[:SS[.fff]]]
    // Count colons; seconds and sub-seconds are the same precision per spec
    var colons: u8 = 0;
    for (t) |c| {
        if (c == ':') colons += 1;
    }
    return colons + 1; // 0 colons = hours(1), 1 colon = minutes(2), 2+ colons = seconds(3)
}

/// Returns a precision level for dateTime strings
fn dateTimePrecisionLevel(dt: []const u8) u8 {
    // Find the T separator
    const t_pos = std.mem.indexOfScalar(u8, dt, 'T') orelse {
        // Date-only portion: count hyphens (YYYY=0, YYYY-MM=1, YYYY-MM-DD=2)
        var hyphens: u8 = 0;
        for (dt) |c| {
            if (c == '-') hyphens += 1;
        }
        return hyphens; // 0=year, 1=month, 2=day
    };
    // Has time component: base precision 3 (day) + time precision
    const time_part = dt[t_pos + 1 ..];
    // Strip timezone suffix for precision calculation
    var end = time_part.len;
    if (end > 0) {
        // Check for Z or +/-offset
        if (time_part[end - 1] == 'Z') {
            end -= 1;
        } else {
            // Check for +HH:MM or -HH:MM at end
            if (end >= 6 and (time_part[end - 6] == '+' or time_part[end - 6] == '-')) {
                end -= 6;
            }
        }
    }
    const clean_time = time_part[0..end];
    return 2 + timePrecisionLevel(clean_time); // day(2) + time precision
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
    // Empty propagates
    if (left.len == 0 or right.len == 0) return out;
    // Multi-item is an error per spec: singleton evaluation of collections
    if (left.len > 1 or right.len > 1) return error.SingletonRequired;

    const va = itemToValue(ctx, left[0]);
    const vb = itemToValue(ctx, right[0]);

    // String concatenation with + operator
    if (op == .add and va == .string and vb == .string) {
        const result = try std.fmt.allocPrint(ctx.allocator, "{s}{s}", .{ va.string, vb.string });
        try out.append(ctx.allocator, makeStringItem(ctx, result));
        return out;
    }

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
            // Integer division: use i128 scaled arithmetic for exact result
            const div_scale: u32 = 8;
            const a128: i128 = @intCast(a);
            const factor = pow10i128(div_scale) orelse {
                return out;
            };
            const numerator = @mulWithOverflow(a128, factor);
            if (numerator[1] != 0) return out;
            const b128: i128 = @intCast(b);
            const result_val = @divTrunc(numerator[0], b128);
            const text = try formatScaledDecimal(ctx.allocator, result_val, div_scale);
            try out.append(ctx.allocator, makeDecimalItemText(ctx, text));
            return out;
        }
    }

    // Long arithmetic (Long + Long, Long + Integer, Integer + Long)
    const a_is_int_or_long = va == .integer or va == .long;
    const b_is_int_or_long = vb == .integer or vb == .long;
    const either_long = va == .long or vb == .long;
    if (a_is_int_or_long and b_is_int_or_long and either_long) {
        const a: i64 = switch (va) {
            .integer => va.integer,
            .long => va.long,
            else => unreachable,
        };
        const b: i64 = switch (vb) {
            .integer => vb.integer,
            .long => vb.long,
            else => unreachable,
        };
        const result: ?i64 = switch (op) {
            .add => a +% b,
            .sub => a -% b,
            .mul => a *% b,
            .int_div => if (b != 0) @divTrunc(a, b) else null,
            .mod => if (b != 0) @mod(a, b) else null,
            .div => null, // Long division with / returns decimal
        };
        if (result) |r| {
            try out.append(ctx.allocator, makeLongItemFromValue(ctx, r));
            return out;
        }
        if (op == .div and b != 0) {
            // Long division: use i128 scaled arithmetic for exact result
            const div_scale: u32 = 8;
            const a128: i128 = @intCast(a);
            const factor = pow10i128(div_scale) orelse {
                return out;
            };
            const numerator = @mulWithOverflow(a128, factor);
            if (numerator[1] != 0) return out;
            const b128: i128 = @intCast(b);
            const result_val = @divTrunc(numerator[0], b128);
            const text = try formatScaledDecimal(ctx.allocator, result_val, div_scale);
            try out.append(ctx.allocator, makeDecimalItemText(ctx, text));
            return out;
        }
    }

    // Decimal arithmetic (i128 scaled-integer)
    // Get text representation and scale for each operand
    const a_text: ?[]const u8 = switch (va) {
        .decimal => |d| d,
        .integer => |i| blk: {
            break :blk std.fmt.allocPrint(ctx.allocator, "{d}", .{i}) catch null;
        },
        .long => |i| blk: {
            break :blk std.fmt.allocPrint(ctx.allocator, "{d}", .{i}) catch null;
        },
        else => null,
    };
    const b_text: ?[]const u8 = switch (vb) {
        .decimal => |d| d,
        .integer => |i| blk: {
            break :blk std.fmt.allocPrint(ctx.allocator, "{d}", .{i}) catch null;
        },
        .long => |i| blk: {
            break :blk std.fmt.allocPrint(ctx.allocator, "{d}", .{i}) catch null;
        },
        else => null,
    };

    if (a_text != null and b_text != null) {
        const at = a_text.?;
        const bt = b_text.?;
        const scale_a = decimalScale(at) orelse 0;
        const scale_b = decimalScale(bt) orelse 0;

        // Determine result scale based on operation
        const result_scale: u32 = switch (op) {
            .add, .sub => @max(scale_a, scale_b),
            .mul => @min(scale_a + scale_b, 8),
            .div => @max(scale_a, 8),
            .int_div => 0,
            .mod => @max(scale_a, scale_b),
        };

        switch (op) {
            .add, .sub => {
                const common = @max(scale_a, scale_b);
                const ia = parseDecimalScaled(at, common) orelse return out;
                const ib = parseDecimalScaled(bt, common) orelse return out;
                const r: i128 = if (op == .add)
                    ia + ib
                else
                    ia - ib;
                const text = try formatScaledDecimal(ctx.allocator, r, result_scale);
                try out.append(ctx.allocator, makeDecimalItemText(ctx, text));
            },
            .mul => {
                // Parse each at its own scale, multiply, result scale = scale_a + scale_b (capped at 8)
                const ia = parseDecimalScaled(at, scale_a) orelse return out;
                const ib = parseDecimalScaled(bt, scale_b) orelse return out;
                const product = @mulWithOverflow(ia, ib);
                if (product[1] != 0) return out;
                const raw_scale = scale_a + scale_b;
                // If raw_scale > 8, we need to divide out the excess
                if (raw_scale > 8) {
                    const excess = raw_scale - 8;
                    const divisor = pow10i128(excess) orelse return out;
                    const r = @divTrunc(product[0], divisor);
                    const text = try formatScaledDecimal(ctx.allocator, r, 8);
                    try out.append(ctx.allocator, makeDecimalItemText(ctx, text));
                } else {
                    const text = try formatScaledDecimal(ctx.allocator, product[0], result_scale);
                    try out.append(ctx.allocator, makeDecimalItemText(ctx, text));
                }
            },
            .div => {
                // Check for division by zero
                const ib_check = parseDecimalScaled(bt, scale_b) orelse return out;
                if (ib_check == 0) return out;
                // Parse both at common scale, then upscale numerator by result_scale
                const common = @max(scale_a, scale_b);
                const ia = parseDecimalScaled(at, common + result_scale) orelse return out;
                const ib = parseDecimalScaled(bt, common) orelse return out;
                const r = @divTrunc(ia, ib);
                const text = try formatScaledDecimal(ctx.allocator, r, result_scale);
                try out.append(ctx.allocator, makeDecimalItemText(ctx, text));
            },
            .int_div => {
                const ib_check = parseDecimalScaled(bt, scale_b) orelse return out;
                if (ib_check == 0) return out;
                const common = @max(scale_a, scale_b);
                const ia = parseDecimalScaled(at, common) orelse return out;
                const ib = parseDecimalScaled(bt, common) orelse return out;
                const r = @divTrunc(ia, ib);
                // int_div returns integer
                const int_val: i64 = @intCast(r);
                try out.append(ctx.allocator, makeIntegerItem(ctx, int_val));
            },
            .mod => {
                const ib_check = parseDecimalScaled(bt, scale_b) orelse return out;
                if (ib_check == 0) return out;
                const common = @max(scale_a, scale_b);
                const ia = parseDecimalScaled(at, common) orelse return out;
                const ib = parseDecimalScaled(bt, common) orelse return out;
                // Use truncated division remainder (matches @rem behavior for consistency)
                const r = @rem(ia, ib);
                const text = try formatScaledDecimal(ctx.allocator, r, result_scale);
                try out.append(ctx.allocator, makeDecimalItemText(ctx, text));
            },
        }
    }

    // Quantity arithmetic
    if (va == .quantity or vb == .quantity) {
        const result_item = try evalQuantityArithmetic(ctx, va, vb, op);
        if (result_item) |r| {
            try out.append(ctx.allocator, r);
            return out;
        }
    }

    // Date/DateTime/Time arithmetic with Quantity
    if ((op == .add or op == .sub) and vb == .quantity) {
        const result_item = try evalDateTimeArithmetic(ctx, va, vb.quantity, op == .add);
        if (result_item) |r| {
            try out.append(ctx.allocator, r);
        }
    }

    return out;
}

// Normalize a calendar duration keyword to singular form for comparison
fn normalizeCalendarUnit(unit: []const u8) []const u8 {
    // Map plural to singular for calendar durations
    const mappings = [_]struct { plural: []const u8, singular: []const u8 }{
        .{ .plural = "years", .singular = "year" },
        .{ .plural = "months", .singular = "month" },
        .{ .plural = "weeks", .singular = "week" },
        .{ .plural = "days", .singular = "day" },
        .{ .plural = "hours", .singular = "hour" },
        .{ .plural = "minutes", .singular = "minute" },
        .{ .plural = "seconds", .singular = "second" },
        .{ .plural = "milliseconds", .singular = "millisecond" },
    };
    for (mappings) |m| {
        if (std.mem.eql(u8, unit, m.plural)) return m.singular;
    }
    return unit;
}

// Map UCUM time units to calendar duration keywords
// Returns the calendar equivalent, or null if not a UCUM time unit
fn ucumToCalendar(unit: []const u8) ?[]const u8 {
    const mappings = [_]struct { ucum: []const u8, calendar: []const u8 }{
        .{ .ucum = "d", .calendar = "day" },
        .{ .ucum = "wk", .calendar = "week" },
        .{ .ucum = "h", .calendar = "hour" },
        .{ .ucum = "min", .calendar = "minute" },
        .{ .ucum = "s", .calendar = "second" },
        .{ .ucum = "ms", .calendar = "millisecond" },
    };
    for (mappings) |m| {
        if (std.mem.eql(u8, unit, m.ucum)) return m.calendar;
    }
    return null;
}

// Check if unit is a calendar duration (after normalization to singular)
fn isCalendarUnit(unit: []const u8) bool {
    const calendar_units = [_][]const u8{
        "year", "month", "week", "day",
        "hour", "minute", "second", "millisecond",
    };
    for (calendar_units) |cu| {
        if (std.mem.eql(u8, unit, cu)) return true;
    }
    return false;
}

// Check if this is a UCUM month or year (which are uncomparable with calendar months/years)
fn isUcumMonthOrYear(unit: []const u8) bool {
    return std.mem.eql(u8, unit, "mo") or std.mem.eql(u8, unit, "a");
}

// Check if this is a calendar month or year
fn isCalendarMonthOrYear(unit: []const u8) bool {
    return std.mem.eql(u8, unit, "month") or std.mem.eql(u8, unit, "year");
}

/// Convert a calendar duration quantity to milliseconds for comparison.
/// Returns null if the unit is not a calendar/UCUM time unit at or below week.
/// year/month cannot be converted because they have variable lengths.
fn calendarToMilliseconds(value: []const u8, unit: []const u8) ?f64 {
    const v = std.fmt.parseFloat(f64, value) catch return null;
    const normalized = normalizeCalendarUnit(unit);

    // Direct calendar units
    const ms_per_second: f64 = 1000.0;
    const ms_per_minute: f64 = 60.0 * ms_per_second;
    const ms_per_hour: f64 = 60.0 * ms_per_minute;
    const ms_per_day: f64 = 24.0 * ms_per_hour;
    const ms_per_week: f64 = 7.0 * ms_per_day;

    // Calendar keywords (normalized to singular)
    if (std.mem.eql(u8, normalized, "week")) return v * ms_per_week;
    if (std.mem.eql(u8, normalized, "day")) return v * ms_per_day;
    if (std.mem.eql(u8, normalized, "hour")) return v * ms_per_hour;
    if (std.mem.eql(u8, normalized, "minute")) return v * ms_per_minute;
    if (std.mem.eql(u8, normalized, "second")) return v * ms_per_second;
    if (std.mem.eql(u8, normalized, "millisecond")) return v;

    // UCUM units
    if (std.mem.eql(u8, normalized, "wk")) return v * ms_per_week;
    if (std.mem.eql(u8, normalized, "d")) return v * ms_per_day;
    if (std.mem.eql(u8, normalized, "h")) return v * ms_per_hour;
    if (std.mem.eql(u8, normalized, "min")) return v * ms_per_minute;
    if (std.mem.eql(u8, normalized, "s")) return v * ms_per_second;
    if (std.mem.eql(u8, normalized, "ms")) return v;

    // year/month are NOT convertible (variable length)
    return null;
}

/// Compare two quantities for equality, handling calendar duration unit conversion.
/// Returns true if equal, false if not equal, null if uncomparable.
fn compareQuantitiesEqual(q_a: item.Quantity, q_b: item.Quantity) ?bool {
    const compat = checkQuantityUnitCompatibility(q_a.unit, q_b.unit);
    switch (compat) {
        .incompatible => return false,
        .uncomparable => return null, // empty for month/year calendar vs UCUM
        .compatible => {
            // If units are exactly the same (after normalization), compare directly
            const na = normalizeCalendarUnit(q_a.unit);
            const nb = normalizeCalendarUnit(q_b.unit);
            if (std.mem.eql(u8, na, nb)) {
                return compareDecimalStrings(q_a.value, q_b.value) == 0;
            }

            // Try to convert both to milliseconds for comparison
            const a_ms = calendarToMilliseconds(q_a.value, q_a.unit);
            const b_ms = calendarToMilliseconds(q_b.value, q_b.unit);
            if (a_ms != null and b_ms != null) {
                return a_ms.? == b_ms.?;
            }

            // Fall back to string comparison (same units)
            return compareDecimalStrings(q_a.value, q_b.value) == 0;
        },
    }
}

/// Compare two quantities for ordering, handling calendar duration unit conversion.
/// Returns -1 (less), 0 (equal), 1 (greater), or null if uncomparable.
fn compareQuantitiesOrdered(q_a: item.Quantity, q_b: item.Quantity) ?i32 {
    const compat = checkQuantityUnitCompatibility(q_a.unit, q_b.unit);
    switch (compat) {
        .incompatible => return null, // can't order different types
        .uncomparable => return null, // empty for month/year calendar vs UCUM
        .compatible => {
            // Check if units need conversion (different calendar time units)
            const na = normalizeCalendarUnit(q_a.unit);
            const nb = normalizeCalendarUnit(q_b.unit);
            if (!std.mem.eql(u8, na, nb)) {
                // Different units - try to convert both to milliseconds
                const a_ms = calendarToMilliseconds(q_a.value, q_a.unit);
                const b_ms = calendarToMilliseconds(q_b.value, q_b.unit);
                if (a_ms != null and b_ms != null) {
                    if (a_ms.? < b_ms.?) return -1;
                    if (a_ms.? > b_ms.?) return 1;
                    return 0;
                }
            }

            // Same units (after normalization) - compare as numbers
            const qa = std.fmt.parseFloat(f64, q_a.value) catch return null;
            const qb = std.fmt.parseFloat(f64, q_b.value) catch return null;
            if (qa < qb) return -1;
            if (qa > qb) return 1;
            return 0;
        },
    }
}

/// Result of unit compatibility check for quantity comparison
const UnitCompatibility = enum {
    compatible, // Units are the same or convertible (comparison proceeds)
    incompatible, // Units are different types (comparison returns false)
    uncomparable, // Units are time-valued but above days/weeks (comparison returns empty)
};

// Check if two quantity units are compatible (same unit, accounting for singular/plural and UCUM)
// Returns tri-state: compatible (proceed), incompatible (false), uncomparable (empty)
/// Check if a unit is a calendar/UCUM time unit at or below week level (convertible)
fn isConvertibleTimeUnit(unit: []const u8) bool {
    const convertible_units = [_][]const u8{
        // Calendar keywords (singular)
        "week", "day", "hour", "minute", "second", "millisecond",
        // UCUM units
        "wk", "d", "h", "min", "s", "ms",
    };
    const normalized = normalizeCalendarUnit(unit);
    for (convertible_units) |cu| {
        if (std.mem.eql(u8, normalized, cu)) return true;
    }
    return false;
}

fn checkQuantityUnitCompatibility(unit_a: []const u8, unit_b: []const u8) UnitCompatibility {
    // Exact match
    if (std.mem.eql(u8, unit_a, unit_b)) return .compatible;

    // Normalize calendar duration units to singular
    const na = normalizeCalendarUnit(unit_a);
    const nb = normalizeCalendarUnit(unit_b);

    // Check after normalization
    if (std.mem.eql(u8, na, nb)) return .compatible;

    // Check UCUM to calendar mappings for comparable time units
    // (d <-> day, wk <-> week, h <-> hour, min <-> minute, s <-> second, ms <-> millisecond)
    if (ucumToCalendar(na)) |cal_a| {
        if (std.mem.eql(u8, cal_a, nb)) return .compatible;
    }
    if (ucumToCalendar(nb)) |cal_b| {
        if (std.mem.eql(u8, na, cal_b)) return .compatible;
    }

    // Special case: UCUM month/year ('mo'/'a') vs calendar month/year
    // Per spec: "calendar durations and definite quantity durations above days (and weeks)
    // are considered un-comparable" - returns empty, not false
    if ((isUcumMonthOrYear(na) and isCalendarMonthOrYear(nb)) or
        (isCalendarMonthOrYear(na) and isUcumMonthOrYear(nb)))
    {
        return .uncomparable;
    }

    // Calendar/UCUM time units at or below week level are compatible (can be converted)
    // Per R5 testQuantity5: 7 days = 1 week is true
    if (isConvertibleTimeUnit(na) and isConvertibleTimeUnit(nb)) {
        return .compatible;
    }

    // Different units
    return .incompatible;
}

// Simple boolean compatibility check (for cases where we don't need tri-state)
fn quantityUnitsCompatible(unit_a: []const u8, unit_b: []const u8) bool {
    return checkQuantityUnitCompatibility(unit_a, unit_b) == .compatible;
}

// Check if two quantity units are equivalent for the ~ operator.
// For equivalence, calendar durations and UCUM durations ARE equivalent per spec:
// "1 year ~ 1 'a' // true"
// "1 second ~ 1 's' // true"
fn quantityUnitsEquivalent(unit_a: []const u8, unit_b: []const u8) bool {
    // Exact match
    if (std.mem.eql(u8, unit_a, unit_b)) return true;

    // Normalize calendar duration units to singular
    const na = normalizeCalendarUnit(unit_a);
    const nb = normalizeCalendarUnit(unit_b);

    // Check after normalization
    if (std.mem.eql(u8, na, nb)) return true;

    // Check UCUM to calendar mappings for time units
    // These are equivalent for the ~ operator per spec
    if (ucumToCalendar(na)) |cal_a| {
        if (std.mem.eql(u8, cal_a, nb)) return true;
    }
    if (ucumToCalendar(nb)) |cal_b| {
        if (std.mem.eql(u8, na, cal_b)) return true;
    }

    // For equivalence, also map year/month UCUM to calendar
    // 'a' <-> year, 'mo' <-> month (equivalence only, not equality)
    const ucum_to_cal_large = [_]struct { ucum: []const u8, calendar: []const u8 }{
        .{ .ucum = "a", .calendar = "year" },
        .{ .ucum = "mo", .calendar = "month" },
    };
    for (ucum_to_cal_large) |m| {
        if (std.mem.eql(u8, na, m.ucum) and std.mem.eql(u8, nb, m.calendar)) return true;
        if (std.mem.eql(u8, na, m.calendar) and std.mem.eql(u8, nb, m.ucum)) return true;
    }

    return false;
}

// Quantity arithmetic: quantity+quantity, quantity*number, quantity/number, etc.
fn evalQuantityArithmetic(
    ctx: anytype,
    va: item.Value,
    vb: item.Value,
    op: ArithOp,
) EvalError!?item.Item {
    // Helper to extract numeric value from integer, decimal, or quantity
    const qa: ?item.Quantity = if (va == .quantity) va.quantity else null;
    const qb: ?item.Quantity = if (vb == .quantity) vb.quantity else null;

    // Extract numeric values
    const na: ?f64 = if (qa) |q| std.fmt.parseFloat(f64, q.value) catch null
        else switch (va) {
            .integer => |i| @floatFromInt(i),
            .decimal => |d| std.fmt.parseFloat(f64, d) catch null,
            else => null,
        };
    const nb: ?f64 = if (qb) |q| std.fmt.parseFloat(f64, q.value) catch null
        else switch (vb) {
            .integer => |i| @floatFromInt(i),
            .decimal => |d| std.fmt.parseFloat(f64, d) catch null,
            else => null,
        };

    if (na == null or nb == null) return null;

    const a = na.?;
    const b = nb.?;

    switch (op) {
        .add, .sub => {
            // Both must be quantities with same unit
            if (qa == null or qb == null) return null;
            if (!quantityUnitsCompatible(qa.?.unit, qb.?.unit)) return null;
            const result = if (op == .add) a + b else a - b;
            var buf: [64]u8 = undefined;
            const result_str = formatDecimal(&buf, result);
            const owned = try ctx.allocator.dupe(u8, result_str);
            return makeQuantityItem(ctx, owned, qa.?.unit);
        },
        .mul => {
            if (qa != null and qb != null) {
                // quantity * quantity => derived unit (e.g., cm * cm = cm2)
                const result = a * b;
                var buf: [64]u8 = undefined;
                const result_str = formatDecimal(&buf, result);
                const owned_val = try ctx.allocator.dupe(u8, result_str);
                const derived_unit = try deriveMultiplyUnit(ctx.allocator, qa.?.unit, qb.?.unit);
                return makeQuantityItem(ctx, owned_val, derived_unit);
            } else {
                // quantity * number or number * quantity
                const q = qa orelse qb orelse return null;
                const result = a * b;
                var buf: [64]u8 = undefined;
                const result_str = formatDecimal(&buf, result);
                const owned_val = try ctx.allocator.dupe(u8, result_str);
                return makeQuantityItem(ctx, owned_val, q.unit);
            }
        },
        .div => {
            if (b == 0) return null; // division by zero => empty
            if (qa != null and qb != null) {
                // quantity / quantity
                if (quantityUnitsCompatible(qa.?.unit, qb.?.unit)) {
                    // Same unit => dimensionless decimal
                    return try makeDecimalItem(ctx, a / b);
                }
                // Different units => try to derive unit (e.g., cm2 / cm = cm)
                const result = a / b;
                var buf: [64]u8 = undefined;
                const result_str = formatDecimal(&buf, result);
                const owned_val = try ctx.allocator.dupe(u8, result_str);
                const derived_unit = try deriveDivideUnit(ctx.allocator, qa.?.unit, qb.?.unit);
                return makeQuantityItem(ctx, owned_val, derived_unit);
            } else if (qa != null) {
                // quantity / number
                const result = a / b;
                var buf: [64]u8 = undefined;
                const result_str = formatDecimal(&buf, result);
                const owned_val = try ctx.allocator.dupe(u8, result_str);
                return makeQuantityItem(ctx, owned_val, qa.?.unit);
            }
            return null;
        },
        .int_div, .mod => {
            // Not commonly used with quantities, but handle quantity / quantity same unit
            if (qa != null and qb != null and quantityUnitsCompatible(qa.?.unit, qb.?.unit)) {
                if (b == 0) return null;
                const result = if (op == .int_div) @trunc(a / b) else @mod(a, b);
                return try makeDecimalItem(ctx, result);
            }
            return null;
        },
    }
}

// Derive unit for quantity multiplication (e.g., cm * cm = cm2)
fn deriveMultiplyUnit(allocator: std.mem.Allocator, unit_a: []const u8, unit_b: []const u8) ![]const u8 {
    if (std.mem.eql(u8, unit_a, unit_b)) {
        // Same unit: e.g., cm * cm = cm2
        return try std.fmt.allocPrint(allocator, "{s}2", .{unit_a});
    }
    // For different units, try simple derived unit notation
    // e.g., cm * cm2 = cm3 (check if unit_b is unit_a + digit)
    if (unit_b.len > unit_a.len and std.mem.startsWith(u8, unit_b, unit_a)) {
        const suffix = unit_b[unit_a.len..];
        if (suffix.len == 1 and suffix[0] >= '2' and suffix[0] <= '8') {
            const new_exp = suffix[0] - '0' + 1;
            return try std.fmt.allocPrint(allocator, "{s}{d}", .{ unit_a, new_exp });
        }
    }
    if (unit_a.len > unit_b.len and std.mem.startsWith(u8, unit_a, unit_b)) {
        const suffix = unit_a[unit_b.len..];
        if (suffix.len == 1 and suffix[0] >= '2' and suffix[0] <= '8') {
            const new_exp = suffix[0] - '0' + 1;
            return try std.fmt.allocPrint(allocator, "{s}{d}", .{ unit_b, new_exp });
        }
    }
    // Fallback: concatenate with dot
    return try std.fmt.allocPrint(allocator, "{s}.{s}", .{ unit_a, unit_b });
}

// Derive unit for quantity division (e.g., cm2 / cm = cm)
fn deriveDivideUnit(allocator: std.mem.Allocator, unit_a: []const u8, unit_b: []const u8) ![]const u8 {
    // Check if unit_a is unit_b + exponent (e.g., cm2 / cm = cm)
    if (unit_a.len > unit_b.len and std.mem.startsWith(u8, unit_a, unit_b)) {
        const suffix = unit_a[unit_b.len..];
        if (suffix.len == 1 and suffix[0] >= '2' and suffix[0] <= '9') {
            const exp = suffix[0] - '0';
            if (exp == 2) {
                return try allocator.dupe(u8, unit_b);
            }
            return try std.fmt.allocPrint(allocator, "{s}{d}", .{ unit_b, exp - 1 });
        }
    }
    // Fallback: use / notation
    return try std.fmt.allocPrint(allocator, "{s}/{s}", .{ unit_a, unit_b });
}

// Date/Time arithmetic with calendar durations (year, month, week, day, hour, minute, second, millisecond)
fn evalDateTimeArithmetic(
    ctx: anytype,
    temporal: item.Value,
    qty: item.Quantity,
    is_add: bool,
) EvalError!?item.Item {
    // Parse the quantity value
    const qty_val = std.fmt.parseFloat(f64, qty.value) catch return null;

    // Check for UCUM 'mo' (mean gregorian month) and 'a' (mean gregorian year)
    // These are NOT equivalent to calendar months/years and should error per R5 tests
    if (std.mem.eql(u8, qty.unit, "mo") or std.mem.eql(u8, qty.unit, "a")) {
        return error.InvalidOperand; // Incompatible unit for date arithmetic
    }

    // Handle fractional UCUM seconds - convert to milliseconds
    // Per R5 testPlusDate19: 0.1 's' = 100 ms
    if (std.mem.eql(u8, qty.unit, "s")) {
        const ms_val = qty_val * 1000.0;
        const ms_amount: i32 = @intFromFloat(if (ms_val < 0) -@round(-ms_val) else @round(ms_val));
        const effective_ms: i32 = if (is_add) ms_amount else -ms_amount;
        switch (temporal) {
            .dateTime => |dt| {
                const result = try addToDateTime(ctx.allocator, dt, effective_ms, .millisecond) orelse return null;
                return makeDateTimeItem(ctx, result);
            },
            .time => |t| {
                const result = try addToTime(ctx.allocator, t, effective_ms, .millisecond) orelse return null;
                return makeTimeItem(ctx, result);
            },
            else => return null,
        }
    }

    // For calendar units, use integer portion only (per spec: decimal is ignored for calendar units)
    const amount: i32 = @intFromFloat(if (qty_val < 0) -@trunc(-qty_val) else @trunc(qty_val));
    const effective_amount: i32 = if (is_add) amount else -amount;

    // Determine the time unit
    const unit = parseTimeUnit(qty.unit) orelse return null;

    // Handle based on temporal type
    switch (temporal) {
        .date => |d| {
            const result = try addToDate(ctx.allocator, d, effective_amount, unit) orelse return null;
            return makeDateItem(ctx, result);
        },
        .dateTime => |dt| {
            const result = try addToDateTime(ctx.allocator, dt, effective_amount, unit) orelse return null;
            return makeDateTimeItem(ctx, result);
        },
        .time => |t| {
            const result = try addToTime(ctx.allocator, t, effective_amount, unit) orelse return null;
            return makeTimeItem(ctx, result);
        },
        else => return null,
    }
}

const TimeUnit = enum {
    year,
    month,
    week,
    day,
    hour,
    minute,
    second,
    millisecond,
};

fn parseTimeUnit(unit: []const u8) ?TimeUnit {
    // Calendar duration keywords
    if (std.mem.eql(u8, unit, "year") or std.mem.eql(u8, unit, "years")) return .year;
    if (std.mem.eql(u8, unit, "month") or std.mem.eql(u8, unit, "months")) return .month;
    if (std.mem.eql(u8, unit, "week") or std.mem.eql(u8, unit, "weeks") or std.mem.eql(u8, unit, "wk")) return .week;
    if (std.mem.eql(u8, unit, "day") or std.mem.eql(u8, unit, "days") or std.mem.eql(u8, unit, "d")) return .day;
    if (std.mem.eql(u8, unit, "hour") or std.mem.eql(u8, unit, "hours") or std.mem.eql(u8, unit, "h")) return .hour;
    if (std.mem.eql(u8, unit, "minute") or std.mem.eql(u8, unit, "minutes") or std.mem.eql(u8, unit, "min")) return .minute;
    if (std.mem.eql(u8, unit, "second") or std.mem.eql(u8, unit, "seconds") or std.mem.eql(u8, unit, "s")) return .second;
    if (std.mem.eql(u8, unit, "millisecond") or std.mem.eql(u8, unit, "milliseconds") or std.mem.eql(u8, unit, "ms")) return .millisecond;
    return null;
}

// Add amount to a date string (format: YYYY, YYYY-MM, or YYYY-MM-DD)
fn addToDate(allocator: std.mem.Allocator, date_str: []const u8, amount: i32, unit: TimeUnit) !?[]const u8 {
    // Parse date components
    var year: i32 = 0;
    var month: i32 = 1;
    var day: i32 = 1;
    var precision: u8 = 1; // 1=year, 2=month, 3=day

    var parts: [3][]const u8 = undefined;
    var part_count: usize = 0;
    var iter = std.mem.splitScalar(u8, date_str, '-');
    while (iter.next()) |part| {
        if (part_count >= 3) break;
        parts[part_count] = part;
        part_count += 1;
    }

    if (part_count >= 1) year = std.fmt.parseInt(i32, parts[0], 10) catch return null;
    if (part_count >= 2) {
        month = std.fmt.parseInt(i32, parts[1], 10) catch return null;
        precision = 2;
    }
    if (part_count >= 3) {
        day = std.fmt.parseInt(i32, parts[2], 10) catch return null;
        precision = 3;
    }

    // Perform the arithmetic
    // Track whether we need to clamp (year/month ops) or normalize days (day/week ops)
    var clamp_day = false;

    // For partial dates, convert quantity to the date's precision first
    var effective_amount = amount;
    var effective_unit = unit;

    if (precision == 1) {
        // Year-only precision: convert everything to years
        switch (unit) {
            .month => {
                effective_amount = @divTrunc(amount, 12);
                effective_unit = .year;
            },
            .week => {
                effective_amount = @divTrunc(amount * 7, 365);
                effective_unit = .year;
            },
            .day => {
                effective_amount = @divTrunc(amount, 365);
                effective_unit = .year;
            },
            else => {},
        }
    } else if (precision == 2) {
        // Year-month precision: convert days/weeks to months
        switch (unit) {
            .week => {
                effective_amount = @divTrunc(amount * 7, 30);
                effective_unit = .month;
            },
            .day => {
                effective_amount = @divTrunc(amount, 30);
                effective_unit = .month;
            },
            else => {},
        }
    }

    switch (effective_unit) {
        .year => {
            year += effective_amount;
            clamp_day = true; // Year ops require clamping to month end
        },
        .month => {
            // Add months and normalize
            const total_months = (year * 12) + (month - 1) + effective_amount;
            year = @divFloor(total_months, 12);
            month = @mod(total_months, 12) + 1;
            clamp_day = true; // Month ops require clamping to month end
        },
        .week => day += effective_amount * 7,
        .day => day += effective_amount,
        else => return null, // Hour/minute/second/millisecond not valid for Date
    }

    // Normalize the date (handle overflow)
    return try normalizeDate(allocator, year, month, day, precision, clamp_day);
}

fn normalizeDate(allocator: std.mem.Allocator, in_year: i32, in_month: i32, in_day: i32, precision: u8, clamp_day: bool) ![]const u8 {
    var year = in_year;
    var month = in_month;
    var day = in_day;

    // Normalize month first
    while (month < 1) {
        month += 12;
        year -= 1;
    }
    while (month > 12) {
        month -= 12;
        year += 1;
    }

    // If precision is year-only, output just year
    if (precision == 1) {
        const y: u32 = @intCast(year);
        return try std.fmt.allocPrint(allocator, "{d:0>4}", .{y});
    }

    // If precision is year-month, output year-month with clamped day
    if (precision == 2) {
        // For year-month precision, we don't output day
        const y: u32 = @intCast(year);
        const m: u32 = @intCast(month);
        return try std.fmt.allocPrint(allocator, "{d:0>4}-{d:0>2}", .{ y, m });
    }

    // For year/month operations, clamp day to the max days in the new month
    // For day/week operations, normalize (roll over to next month)
    if (clamp_day) {
        const max_day = getDaysInMonth(year, month);
        if (day > max_day) day = max_day;
    } else {
        // Normalize day (handle day overflow/underflow)
        while (day < 1) {
            month -= 1;
            if (month < 1) {
                month = 12;
                year -= 1;
            }
            day += getDaysInMonth(year, month);
        }
        while (day > getDaysInMonth(year, month)) {
            day -= getDaysInMonth(year, month);
            month += 1;
            if (month > 12) {
                month = 1;
                year += 1;
            }
        }
    }

    const y: u32 = @intCast(year);
    const m: u32 = @intCast(month);
    const d: u32 = @intCast(day);
    return try std.fmt.allocPrint(allocator, "{d:0>4}-{d:0>2}-{d:0>2}", .{ y, m, d });
}

// Helper to get days in month, returning i32 (uses the existing daysInMonth with proper casting)
fn getDaysInMonth(year: i32, month: i32) i32 {
    if (month < 1 or month > 12) return 30;
    const m: u32 = @intCast(month);
    return @intCast(daysInMonth(year, m) orelse 30);
}

// Add amount to a DateTime string (format: YYYY-MM-DDThh:mm:ss.fff[Z|+hh:mm])
fn addToDateTime(allocator: std.mem.Allocator, dt_str: []const u8, amount: i32, unit: TimeUnit) !?[]const u8 {
    // Split at 'T' to separate date and time parts
    var t_iter = std.mem.splitScalar(u8, dt_str, 'T');
    const date_part = t_iter.next() orelse return null;
    const time_part = t_iter.next(); // May be null for partial DateTime

    // Parse date
    var year: i32 = 0;
    var month: i32 = 1;
    var day: i32 = 1;
    var date_precision: u8 = 1;

    var date_parts: [3][]const u8 = undefined;
    var date_part_count: usize = 0;
    var date_iter = std.mem.splitScalar(u8, date_part, '-');
    while (date_iter.next()) |part| {
        if (date_part_count >= 3) break;
        date_parts[date_part_count] = part;
        date_part_count += 1;
    }

    if (date_part_count >= 1) year = std.fmt.parseInt(i32, date_parts[0], 10) catch return null;
    if (date_part_count >= 2) {
        month = std.fmt.parseInt(i32, date_parts[1], 10) catch return null;
        date_precision = 2;
    }
    if (date_part_count >= 3) {
        day = std.fmt.parseInt(i32, date_parts[2], 10) catch return null;
        date_precision = 3;
    }

    // Parse time if present
    var hour: i32 = 0;
    var minute: i32 = 0;
    var second: i32 = 0;
    var millis: i32 = 0;
    var time_precision: u8 = 0; // 0=no time, 1=hour, 2=minute, 3=second, 4=millisecond
    var timezone: ?[]const u8 = null;

    if (time_part) |tp| {
        var tz_start: usize = tp.len;
        // Find timezone offset
        for (tp, 0..) |c, i| {
            if (c == 'Z' or c == '+' or (c == '-' and i > 0)) {
                tz_start = i;
                timezone = tp[i..];
                break;
            }
        }
        const time_only = tp[0..tz_start];

        // Parse time components
        var time_iter = std.mem.splitScalar(u8, time_only, ':');
        if (time_iter.next()) |h| {
            hour = std.fmt.parseInt(i32, h, 10) catch 0;
            time_precision = 1;
        }
        if (time_iter.next()) |m| {
            minute = std.fmt.parseInt(i32, m, 10) catch 0;
            time_precision = 2;
        }
        if (time_iter.next()) |s_part| {
            // Handle seconds with optional milliseconds
            var sec_iter = std.mem.splitScalar(u8, s_part, '.');
            if (sec_iter.next()) |sec| {
                second = std.fmt.parseInt(i32, sec, 10) catch 0;
                time_precision = 3;
            }
            if (sec_iter.next()) |ms| {
                millis = std.fmt.parseInt(i32, ms, 10) catch 0;
                // Normalize to 3 digits
                if (ms.len == 1) millis *= 100;
                if (ms.len == 2) millis *= 10;
                time_precision = 4;
            }
        }
    }

    // Perform arithmetic
    switch (unit) {
        .year => year += amount,
        .month => {
            const total_months = (year * 12) + (month - 1) + amount;
            year = @divFloor(total_months, 12);
            month = @mod(total_months, 12) + 1;
        },
        .week => day += amount * 7,
        .day => day += amount,
        .hour => hour += amount,
        .minute => minute += amount,
        .second => second += amount,
        .millisecond => millis += amount,
    }

    // Normalize time, cascading to date if necessary
    while (millis < 0) {
        millis += 1000;
        second -= 1;
    }
    while (millis >= 1000) {
        millis -= 1000;
        second += 1;
    }

    while (second < 0) {
        second += 60;
        minute -= 1;
    }
    while (second >= 60) {
        second -= 60;
        minute += 1;
    }

    while (minute < 0) {
        minute += 60;
        hour -= 1;
    }
    while (minute >= 60) {
        minute -= 60;
        hour += 1;
    }

    while (hour < 0) {
        hour += 24;
        day -= 1;
    }
    while (hour >= 24) {
        hour -= 24;
        day += 1;
    }

    // Normalize month first
    while (month < 1) {
        month += 12;
        year -= 1;
    }
    while (month > 12) {
        month -= 12;
        year += 1;
    }

    // Normalize day
    while (day < 1) {
        month -= 1;
        if (month < 1) {
            month = 12;
            year -= 1;
        }
        day += getDaysInMonth(year, month);
    }
    while (day > getDaysInMonth(year, month)) {
        day -= getDaysInMonth(year, month);
        month += 1;
        if (month > 12) {
            month = 1;
            year += 1;
        }
    }

    // Clamp day to max days in month
    const max_day = getDaysInMonth(year, month);
    if (day > max_day) day = max_day;

    // Format output based on precision
    var buf: [64]u8 = undefined;
    var len: usize = 0;

    // Cast to unsigned for formatting
    const y: u32 = @intCast(year);
    const m: u32 = @intCast(month);
    const d: u32 = @intCast(day);
    const h: u32 = @intCast(hour);
    const min: u32 = @intCast(minute);
    const sec: u32 = @intCast(second);
    const ms: u32 = @intCast(millis);

    // Date part
    if (date_precision == 1) {
        len = (std.fmt.bufPrint(&buf, "{d:0>4}", .{y}) catch return null).len;
    } else if (date_precision == 2) {
        len = (std.fmt.bufPrint(&buf, "{d:0>4}-{d:0>2}", .{ y, m }) catch return null).len;
    } else {
        len = (std.fmt.bufPrint(&buf, "{d:0>4}-{d:0>2}-{d:0>2}", .{ y, m, d }) catch return null).len;
    }

    // Add time if present
    if (time_precision >= 1) {
        len += (std.fmt.bufPrint(buf[len..], "T{d:0>2}", .{h}) catch return null).len;
    }
    if (time_precision >= 2) {
        len += (std.fmt.bufPrint(buf[len..], ":{d:0>2}", .{min}) catch return null).len;
    }
    if (time_precision >= 3) {
        len += (std.fmt.bufPrint(buf[len..], ":{d:0>2}", .{sec}) catch return null).len;
    }
    if (time_precision >= 4) {
        len += (std.fmt.bufPrint(buf[len..], ".{d:0>3}", .{ms}) catch return null).len;
    }

    // Add timezone if present
    if (timezone) |tz| {
        @memcpy(buf[len..][0..tz.len], tz);
        len += tz.len;
    }

    return try allocator.dupe(u8, buf[0..len]);
}

// Add amount to a Time string (format: hh:mm:ss.fff, wraps around cyclically)
fn addToTime(allocator: std.mem.Allocator, time_str: []const u8, amount: i32, unit: TimeUnit) !?[]const u8 {
    var hour: i32 = 0;
    var minute: i32 = 0;
    var second: i32 = 0;
    var millis: i32 = 0;
    var time_precision: u8 = 1;

    // Parse time components
    var time_iter = std.mem.splitScalar(u8, time_str, ':');
    if (time_iter.next()) |h| {
        hour = std.fmt.parseInt(i32, h, 10) catch 0;
        time_precision = 1;
    }
    if (time_iter.next()) |m| {
        minute = std.fmt.parseInt(i32, m, 10) catch 0;
        time_precision = 2;
    }
    if (time_iter.next()) |s_part| {
        var sec_iter = std.mem.splitScalar(u8, s_part, '.');
        if (sec_iter.next()) |sec| {
            second = std.fmt.parseInt(i32, sec, 10) catch 0;
            time_precision = 3;
        }
        if (sec_iter.next()) |ms| {
            millis = std.fmt.parseInt(i32, ms, 10) catch 0;
            if (ms.len == 1) millis *= 100;
            if (ms.len == 2) millis *= 10;
            time_precision = 4;
        }
    }

    // Time doesn't support year/month/week/day
    switch (unit) {
        .hour => hour += amount,
        .minute => minute += amount,
        .second => second += amount,
        .millisecond => millis += amount,
        else => return null, // Year/month/week/day not valid for Time
    }

    // Normalize (cyclic for Time values)
    while (millis < 0) {
        millis += 1000;
        second -= 1;
    }
    while (millis >= 1000) {
        millis -= 1000;
        second += 1;
    }

    while (second < 0) {
        second += 60;
        minute -= 1;
    }
    while (second >= 60) {
        second -= 60;
        minute += 1;
    }

    while (minute < 0) {
        minute += 60;
        hour -= 1;
    }
    while (minute >= 60) {
        minute -= 60;
        hour += 1;
    }

    // Cyclic wrap for hour (Time has no date, so wraps around)
    hour = @mod(hour, 24);
    if (hour < 0) hour += 24;

    // Format output
    var buf: [32]u8 = undefined;
    var len: usize = 0;

    // Cast to unsigned for formatting
    const h: u32 = @intCast(hour);
    const min: u32 = @intCast(minute);
    const sec: u32 = @intCast(second);
    const ms: u32 = @intCast(millis);

    len = (std.fmt.bufPrint(&buf, "{d:0>2}", .{h}) catch return null).len;
    if (time_precision >= 2) {
        len += (std.fmt.bufPrint(buf[len..], ":{d:0>2}", .{min}) catch return null).len;
    }
    if (time_precision >= 3) {
        len += (std.fmt.bufPrint(buf[len..], ":{d:0>2}", .{sec}) catch return null).len;
    }
    if (time_precision >= 4) {
        len += (std.fmt.bufPrint(buf[len..], ".{d:0>3}", .{ms}) catch return null).len;
    }

    return try allocator.dupe(u8, buf[0..len]);
}

fn evalConcat(
    ctx: anytype,
    left: []const item.Item,
    right: []const item.Item,
) EvalError!ItemList {
    var out = ItemList.empty;

    // Multi-item is an error per spec: singleton evaluation of collections
    if (left.len > 1 or right.len > 1) return error.SingletonRequired;

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
            // If input is empty, return empty (not false) per official tests
            if (operand.items.len == 0) {
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
    return itemIsTypeImpl(ctx, it, type_name, false);
}

fn itemIsTypeImplicit(ctx: anytype, it: item.Item, type_name: []const u8) bool {
    return itemIsTypeImpl(ctx, it, type_name, true);
}

fn itemIsTypeImpl(ctx: anytype, it: item.Item, type_name: []const u8, allow_implicit: bool) bool {
    // Check if we have a model type match first (for FHIR types like Patient, boolean, etc.)
    if (it.type_id != 0 and ctx.schema != null) {
        const s = ctx.schema.?;
        // Check if the item has a model type - if so, use strict model type matching
        if (schema.isModelType(it.type_id)) {
            // Get the target type_id for the requested type name
            const target_type_id = s.typeIdByLocalName(type_name) orelse
                s.typeIdByQualifiedName(type_name);
            if (target_type_id) |tid| {
                // Check if item's type matches exactly
                if (it.type_id == tid) return true;
                // Check inheritance chain, but NOT for FHIR primitive targets.
                // Per spec: "All primitives are considered to be independent types
                // (so markdown is not a subclass of string)."
                if (s.implicitSystemTypeId(tid) == 0) {
                    if (s.isSubtype(it.type_id, tid)) return true;
                }
            }
            // Check implicit FHIR→System conversion (only for ofType, not is/as)
            if (allow_implicit) {
                const implicit_sys_id = s.implicitSystemTypeId(it.type_id);
                if (implicit_sys_id != 0) {
                    // Resolve the target type name as a System type
                    const target_sys_id = resolveSystemTypeByName(type_name);
                    if (target_sys_id != 0 and implicit_sys_id == target_sys_id) return true;
                }
            }
            // If item has a model type but target type is not in schema,
            // do NOT fall back to JSON-based matching (strict type checking)
            return false;
        }
    }

    // For non-model items, use value-kind or JSON-type based checking
    // Normalize type name (remove System. prefix if present) for System type checks
    const normalized = if (std.mem.startsWith(u8, type_name, "System."))
        type_name[7..]
    else if (std.mem.startsWith(u8, type_name, "FHIR."))
        type_name[5..] // Also strip FHIR. prefix
    else
        type_name;

    // If item carries a well-known System type_id, compare against the target
    if (it.type_id != 0 and !schema.isModelType(it.type_id)) {
        const target_sys_id = resolveSystemTypeByName(type_name);
        if (target_sys_id != 0) return it.type_id == target_sys_id;
    }

    // Check value kind for value items
    if (it.data_kind == .value and it.value != null) {
        return switch (it.value.?) {
            .boolean => std.mem.eql(u8, normalized, "Boolean"),
            .integer => std.mem.eql(u8, normalized, "Integer"),
            .long => std.mem.eql(u8, normalized, "Long"),
            .decimal => std.mem.eql(u8, normalized, "Decimal"),
            .string => std.mem.eql(u8, normalized, "String"),
            .date => std.mem.eql(u8, normalized, "Date"),
            .time => std.mem.eql(u8, normalized, "Time"),
            .dateTime => std.mem.eql(u8, normalized, "DateTime"),
            .quantity => std.mem.eql(u8, normalized, "Quantity"),
            .typeInfo => std.mem.eql(u8, normalized, "TypeInfo"),
            .empty => false,
        };
    }

    // For node-backed values without a model type, check the underlying JSON type
    if (it.data_kind == .node_ref and it.node != null) {
        const A = @TypeOf(ctx.adapter.*);
        const ref = it.node.?;
        return switch (A.kind(ctx.adapter, ref)) {
            .bool => std.mem.eql(u8, normalized, "Boolean"),
            .number => std.mem.eql(u8, normalized, "Integer") or std.mem.eql(u8, normalized, "Decimal"),
            .string => std.mem.eql(u8, normalized, "String"),
            else => false,
        };
    }

    return false;
}

/// Resolve a type name (e.g., "String", "System.String", "DateTime") to a System type id.
/// Returns 0 if the name doesn't refer to a System type.
fn resolveSystemTypeByName(type_name: []const u8) u32 {
    // Try with "System." prefix if not already qualified
    if (std.mem.startsWith(u8, type_name, "System.")) {
        return schema.systemTypeId(type_name) orelse 0;
    }
    // Try constructing "System.X" from bare name
    const system_names = schema.SystemTypeNames;
    for (system_names, 0..) |sys_name, i| {
        // sys_name is "System.Foo", check if type_name matches "Foo"
        if (sys_name.len > 7 and std.mem.eql(u8, sys_name[7..], type_name)) {
            return @intCast(i + 1);
        }
    }
    return 0;
}

/// Check if an item matches a type name (for path resolution)
/// This is used when the first identifier in a path might be a type name.
fn itemMatchesType(ctx: anytype, it: item.Item, type_name: []const u8) bool {
    // For model types, check the type_id
    if (it.type_id != 0 and ctx.schema != null) {
        const s = ctx.schema.?;
        // Get the type name for this item - use schema for model types
        const item_type_name: []const u8 = if (schema.isModelType(it.type_id))
            s.typeName(it.type_id)
        else
            schema.systemTypeName(it.type_id);

        if (item_type_name.len > 0) {
            // Compare local names (e.g., "Patient" matches "FHIR.Patient")
            // Extract local name from qualified name
            const item_local = if (std.mem.indexOf(u8, item_type_name, ".")) |dot|
                item_type_name[dot + 1 ..]
            else
                item_type_name;

            // The type_name might be qualified (FHIR.Patient) or just local (Patient)
            const check_local = if (std.mem.indexOf(u8, type_name, ".")) |dot|
                type_name[dot + 1 ..]
            else
                type_name;

            if (std.mem.eql(u8, item_local, check_local)) return true;

            // Check inheritance chain
            const target_type_id = s.typeIdByLocalName(type_name) orelse
                s.typeIdByQualifiedName(type_name);
            if (target_type_id) |tid| {
                if (s.isSubtype(it.type_id, tid)) return true;
            }
        }
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
                    .long => |val| {
                        try out.append(ctx.allocator, makeLongItemFromValue(ctx, -val));
                    },
                    .decimal => |val| {
                        const f = std.fmt.parseFloat(f64, val) catch return error.InvalidOperand;
                        try out.append(ctx.allocator, try makeDecimalItem(ctx, -f));
                    },
                    .quantity => |q| {
                        const f = std.fmt.parseFloat(f64, q.value) catch return error.InvalidOperand;
                        var buf: [64]u8 = undefined;
                        const neg_str = formatDecimal(&buf, -f);
                        const owned = try ctx.allocator.dupe(u8, neg_str);
                        try out.append(ctx.allocator, makeQuantityItem(ctx, owned, q.unit));
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
                    .long => |v| try out.append(ctx.allocator, makeLongItemFromValue(ctx, -v)),
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
    // Check if any step uses defineVariable - if so, we need a local env
    const needs_local_env = for (inv.steps) |step| {
        switch (step) {
            .Function => |call| {
                if (std.mem.eql(u8, call.name, "defineVariable")) break true;
            },
            else => {},
        }
    } else false;

    // Create local env if needed (for defineVariable support)
    var local_env: ?Env = null;
    defer if (local_env) |*le| le.deinit();

    var effective_env: ?*Env = env;
    if (needs_local_env) {
        local_env = Env.init(ctx.allocator);
        // Copy parent env if it exists
        if (env) |e| {
            var it = e.map.iterator();
            while (it.next()) |entry| {
                try local_env.?.map.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }
        effective_env = &local_env.?;
    }

    // First evaluate the operand
    var current = try evalExpressionCtx(ctx, inv.operand.*, root_item, effective_env, index);
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
                const result = try evalFunction(ctx, call, current.items, effective_env);
                current.deinit(ctx.allocator);
                current = result;
            },
        }
    }

    return current;
}

const FnTag = enum {
    now, today, timeOfDay, duration, difference,
    count, empty, exists,
    toInteger, toBoolean, toDecimal, toLong,
    convertsToLong, convertsToInteger, convertsToBoolean, convertsToDecimal,
    toString, convertsToString,
    toTime, convertsToTime, toDate, convertsToDate, toDateTime, convertsToDateTime,
    toQuantity, convertsToQuantity,
    not, @"is", @"as", ofType, conformsTo,
    distinct, isDistinct,
    allTrue, anyTrue, allFalse, anyFalse,
    sort, select, where, all, repeat, repeatAll,
    children, descendants,
    sum, avg, min, max, aggregate,
    single, @"type", first, last, tail, skip, take,
    startsWith, endsWith, contains, substring,
    abs, ceiling, floor, truncate, round, precision,
    lowBoundary, highBoundary,
    yearOf, monthOf, dayOf, hourOf, minuteOf, secondOf, millisecondOf,
    timezoneOffsetOf, dateOf, timeOf,
    exp, ln, log, sqrt, power,
    iif, coalesce,
    length, indexOf, lastIndexOf, upper, lower, replace,
    matches, matchesFull, replaceMatches, trim, toChars, split, join,
    @"union", combine, intersect, subsetOf, supersetOf, exclude,
    defineVariable, encode, decode, escape, unescape, trace,
};

fn evalFunction(
    ctx: anytype,
    call: ast.FunctionCall,
    input: []const item.Item,
    env: ?*Env,
) EvalError!ItemList {
    const tag = std.meta.stringToEnum(FnTag, call.name) orelse return error.InvalidFunction;
    if (tag == .now) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        const ts = ctx.timestamp;
        const formatted = try formatDateTime(ctx.allocator, ts);
        try out.append(ctx.allocator, makeDateTimeItem(ctx, formatted));
        return out;
    }
    if (tag == .today) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        const ts = ctx.timestamp;
        const formatted = try formatDate(ctx.allocator, ts);
        try out.append(ctx.allocator, makeDateItem(ctx, formatted));
        return out;
    }
    if (tag == .timeOfDay) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        const ts = ctx.timestamp;
        const formatted = try formatTime(ctx.allocator, ts);
        try out.append(ctx.allocator, makeTimeItem(ctx, formatted));
        return out;
    }
    if (tag == .duration) {
        if (call.args.len != 2) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len != 1) return error.SingletonRequired;
        
        var other = try evalExpressionCtx(ctx, call.args[0], ctx.root_item, env, null);
        defer other.deinit(ctx.allocator);
        if (other.items.len == 0) return ItemList.empty;
        if (other.items.len != 1) return error.SingletonRequired;
        
        const precision = try evalStringArg(ctx, call.args[1], env);
        if (precision == null) return ItemList.empty;
        
        return evalDuration(ctx, input[0], other.items[0], precision.?);
    }
    if (tag == .difference) {
        if (call.args.len != 2) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len != 1) return error.SingletonRequired;
        
        var other = try evalExpressionCtx(ctx, call.args[0], ctx.root_item, env, null);
        defer other.deinit(ctx.allocator);
        if (other.items.len == 0) return ItemList.empty;
        if (other.items.len != 1) return error.SingletonRequired;
        
        const precision = try evalStringArg(ctx, call.args[1], env);
        if (precision == null) return ItemList.empty;
        
        return evalDifference(ctx, input[0], other.items[0], precision.?);
    }
    if (tag == .count) {
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(input.len)));
        return out;
    }
    if (tag == .empty) {
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, input.len == 0));
        return out;
    }
    if (tag == .exists) {
        var out = ItemList.empty;
        if (call.args.len == 0) {
            // exists() without criteria: true if collection is non-empty
            try out.append(ctx.allocator, makeBoolItem(ctx, input.len != 0));
            return out;
        }
        // exists(criteria): shorthand for where(criteria).exists()
        // Return true if ANY element matches the criteria
        if (call.args.len != 1) return error.InvalidFunction;
        for (input, 0..) |it, idx| {
            const saved_root = ctx.root_item;
            const saved_index = ctx.current_index;
            ctx.root_item = it;
            ctx.current_index = idx;
            defer ctx.root_item = saved_root;
            defer ctx.current_index = saved_index;
            var criteria = try evalExpressionCtx(ctx, call.args[0], it, env, idx);
            defer criteria.deinit(ctx.allocator);
            if (criteria.items.len == 0) continue;
            if (criteria.items.len != 1) return error.InvalidPredicate;
            const crit = criteria.items[0];
            if (!itemIsBool(ctx, crit)) return error.InvalidPredicate;
            if (itemBoolValue(ctx, crit)) {
                // Found at least one match
                try out.append(ctx.allocator, makeBoolItem(ctx, true));
                return out;
            }
        }
        // No matches found
        try out.append(ctx.allocator, makeBoolItem(ctx, false));
        return out;
    }
    if (tag == .toInteger) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToInteger(ctx, input[0])) |val| {
            try out.append(ctx.allocator, makeIntegerItem(ctx, val));
        }
        return out;
    }
    if (tag == .toBoolean) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToBoolean(ctx, input[0])) |val| {
            try out.append(ctx.allocator, makeBoolItem(ctx, val));
        }
        return out;
    }
    if (tag == .toDecimal) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToDecimalText(ctx, input[0])) |text| {
            try out.append(ctx.allocator, makeDecimalItemText(ctx, text));
        }
        return out;
    }
    if (tag == .toLong) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToLong(ctx, input[0])) |val| {
            try out.append(ctx.allocator, makeLongItemFromValue(ctx, val));
        }
        return out;
    }
    if (tag == .convertsToLong) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = convertToLong(ctx, input[0]) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (tag == .convertsToInteger) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = convertToInteger(ctx, input[0]) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (tag == .convertsToBoolean) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = convertToBoolean(ctx, input[0]) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (tag == .convertsToDecimal) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = canConvertToDecimal(ctx, input[0]);
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (tag == .toString) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToString(ctx, input[0])) |str| {
            try out.append(ctx.allocator, makeStringItem(ctx, str));
        }
        return out;
    }
    if (tag == .convertsToString) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = canConvertToString(ctx, input[0]);
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (tag == .toTime) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToTime(ctx, input[0])) |time_str| {
            try out.append(ctx.allocator, makeTimeItem(ctx, time_str));
        }
        return out;
    }
    if (tag == .convertsToTime) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = convertToTime(ctx, input[0]) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (tag == .toDate) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToDate(ctx, input[0])) |date_str| {
            try out.append(ctx.allocator, makeDateItem(ctx, date_str));
        }
        return out;
    }
    if (tag == .convertsToDate) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = convertToDate(ctx, input[0]) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (tag == .toDateTime) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToDateTime(ctx, input[0])) |dt_str| {
            try out.append(ctx.allocator, makeDateTimeItem(ctx, dt_str));
        }
        return out;
    }
    if (tag == .convertsToDateTime) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const convertible = convertToDateTime(ctx, input[0]) != null;
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (tag == .toQuantity) {
        // toQuantity([unit]) - optional unit argument for conversion
        if (call.args.len > 1) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (convertToQuantity(ctx, input[0])) |q| {
            // If unit argument is provided, we would need to convert
            // For now, we don't support unit conversion - just return empty
            if (call.args.len == 1) {
                // Unit conversion requested - check if same unit, else empty
                const target_unit = try evalExpressionCtx(ctx, call.args[0], ctx.root_item, env, null);
                if (target_unit.items.len == 1) {
                    const target_str = itemStringValue(ctx, target_unit.items[0]);
                    if (target_str) |tu| {
                        if (std.mem.eql(u8, q.unit, tu)) {
                            // Same unit - return as-is
                            try out.append(ctx.allocator, makeQuantityItem(ctx, q.value, q.unit));
                            return out;
                        }
                    }
                }
                // Unit conversion not supported - return empty
                return out;
            }
            try out.append(ctx.allocator, makeQuantityItem(ctx, q.value, q.unit));
        }
        return out;
    }
    if (tag == .convertsToQuantity) {
        // convertsToQuantity([unit]) - optional unit argument
        if (call.args.len > 1) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        var convertible = canConvertToQuantity(ctx, input[0]);
        // If unit argument provided, check if conversion would succeed
        if (convertible and call.args.len == 1) {
            if (convertToQuantity(ctx, input[0])) |q| {
                const target_unit = try evalExpressionCtx(ctx, call.args[0], ctx.root_item, env, null);
                if (target_unit.items.len == 1) {
                    const target_str = itemStringValue(ctx, target_unit.items[0]);
                    if (target_str) |tu| {
                        // Only same unit conversion supported
                        convertible = std.mem.eql(u8, q.unit, tu);
                    } else {
                        convertible = false;
                    }
                } else {
                    convertible = false;
                }
            } else {
                convertible = false;
            }
        }
        try out.append(ctx.allocator, makeBoolItem(ctx, convertible));
        return out;
    }
    if (tag == .not) {
        // not() returns the boolean negation of the input
        // Per FHIRPath spec §5.5.2: input is singleton Boolean
        // Empty input returns empty; multi-item input is a singleton error
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        if (!itemIsBool(ctx, input[0])) {
            // Non-boolean singleton is truthy, so not() = false
            try out.append(ctx.allocator, makeBoolItem(ctx, false));
            return out;
        }
        try out.append(ctx.allocator, makeBoolItem(ctx, !itemBoolValue(ctx, input[0])));
        return out;
    }
    if (tag == .@"is") {
        if (call.args.len != 1) return error.InvalidFunction;
        const type_name = try typeNameFromExpr(ctx, call.args[0]);
        defer ctx.allocator.free(type_name);
        var out = ItemList.empty;
        // Empty input returns empty (not false) per official tests
        if (input.len == 0) {
            return out;
        }
        if (input.len != 1) return error.SingletonRequired;
        try out.append(ctx.allocator, makeBoolItem(ctx, itemIsType(ctx, input[0], type_name)));
        return out;
    }
    if (tag == .@"as") {
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
    if (tag == .ofType) {
        if (call.args.len != 1) return error.InvalidFunction;
        const type_name = try typeNameFromExpr(ctx, call.args[0]);
        defer ctx.allocator.free(type_name);
        var out = ItemList.empty;
        for (input) |it| {
            // ofType uses implicit FHIR→System conversion (spec: "ofType() does not have such restrictions")
            if (itemIsTypeImplicit(ctx, it, type_name)) {
                try out.append(ctx.allocator, it);
            }
        }
        return out;
    }
    // FHIR-specific: conformsTo(profile) checks if resource conforms to a profile URL
    // Returns true if resourceType matches the profile URL's target type
    // Returns false if types don't match
    // Returns empty if: input not singleton, profile is empty, or profile URL is invalid/unknown
    if (tag == .conformsTo) {
        if (call.args.len != 1) return error.InvalidFunction;
        var out = ItemList.empty;

        // Must be exactly one input element
        if (input.len != 1) return out;

        // Get the profile URL argument
        const profile_url = try evalStringArg(ctx, call.args[0], env);
        if (profile_url == null or profile_url.?.len == 0) return out;

        // Extract the type name from the profile URL
        // Standard format: http://hl7.org/fhir/StructureDefinition/{TypeName}
        const sd_prefix = "http://hl7.org/fhir/StructureDefinition/";
        if (!std.mem.startsWith(u8, profile_url.?, sd_prefix)) {
            // Unknown profile URL format - return error per spec
            return error.InvalidFunction;
        }
        const expected_type = profile_url.?[sd_prefix.len..];
        if (expected_type.len == 0) return error.InvalidFunction;

        // Get the actual resource type from the input
        const it = input[0];
        const actual_type: ?[]const u8 = blk: {
            // Check for resourceType in node-backed items
            if (it.data_kind == .node_ref and it.node != null) {
                break :blk resourceTypeName(ctx, it.node.?);
            }
            // For type_id based items, try to get the FHIR type name
            if (it.type_id != 0 and schema.isModelType(it.type_id)) {
                if (ctx.schema) |s| {
                    const full_name = s.typeName(it.type_id);
                    // Strip "FHIR." prefix if present
                    if (std.mem.startsWith(u8, full_name, "FHIR.")) {
                        break :blk full_name[5..];
                    }
                    break :blk full_name;
                }
            }
            break :blk null;
        };

        if (actual_type == null) {
            // Can't determine resource type, return false
            try out.append(ctx.allocator, makeBoolItem(ctx, false));
            return out;
        }

        // Compare types
        const matches = std.mem.eql(u8, actual_type.?, expected_type);
        try out.append(ctx.allocator, makeBoolItem(ctx, matches));
        return out;
    }
    if (tag == .distinct) {
        return distinctItems(ctx, input);
    }
    if (tag == .isDistinct) {
        var distinct = try distinctItems(ctx, input);
        defer distinct.deinit(ctx.allocator);
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, distinct.items.len == input.len));
        return out;
    }
    // Collection boolean aggregation: allTrue, anyTrue, allFalse, anyFalse
    if (tag == .allTrue) {
        if (call.args.len != 0) return error.InvalidFunction;
        // Empty collection returns true (vacuous truth)
        if (input.len == 0) {
            var out = ItemList.empty;
            try out.append(ctx.allocator, makeBoolItem(ctx, true));
            return out;
        }
        // All items must be boolean; error if non-boolean found
        for (input) |it| {
            if (!itemIsBool(ctx, it)) return error.InvalidOperand;
            if (!itemBoolValue(ctx, it)) {
                var out = ItemList.empty;
                try out.append(ctx.allocator, makeBoolItem(ctx, false));
                return out;
            }
        }
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, true));
        return out;
    }
    if (tag == .anyTrue) {
        if (call.args.len != 0) return error.InvalidFunction;
        // Empty collection returns false
        if (input.len == 0) {
            var out = ItemList.empty;
            try out.append(ctx.allocator, makeBoolItem(ctx, false));
            return out;
        }
        // All items must be boolean; error if non-boolean found
        for (input) |it| {
            if (!itemIsBool(ctx, it)) return error.InvalidOperand;
            if (itemBoolValue(ctx, it)) {
                var out = ItemList.empty;
                try out.append(ctx.allocator, makeBoolItem(ctx, true));
                return out;
            }
        }
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, false));
        return out;
    }
    if (tag == .allFalse) {
        if (call.args.len != 0) return error.InvalidFunction;
        // Empty collection returns true (vacuous truth)
        if (input.len == 0) {
            var out = ItemList.empty;
            try out.append(ctx.allocator, makeBoolItem(ctx, true));
            return out;
        }
        // All items must be boolean; error if non-boolean found
        for (input) |it| {
            if (!itemIsBool(ctx, it)) return error.InvalidOperand;
            if (itemBoolValue(ctx, it)) {
                var out = ItemList.empty;
                try out.append(ctx.allocator, makeBoolItem(ctx, false));
                return out;
            }
        }
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, true));
        return out;
    }
    if (tag == .anyFalse) {
        if (call.args.len != 0) return error.InvalidFunction;
        // Empty collection returns false
        if (input.len == 0) {
            var out = ItemList.empty;
            try out.append(ctx.allocator, makeBoolItem(ctx, false));
            return out;
        }
        // All items must be boolean; error if non-boolean found
        for (input) |it| {
            if (!itemIsBool(ctx, it)) return error.InvalidOperand;
            if (!itemBoolValue(ctx, it)) {
                var out = ItemList.empty;
                try out.append(ctx.allocator, makeBoolItem(ctx, true));
                return out;
            }
        }
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, false));
        return out;
    }
    if (tag == .sort) {
        // sort([keySelector, ...]) : collection
        // Sorts the input collection using key selector expressions.
        // If no key selector is provided, sorts using default ordering.
        // Multiple keys: first key is primary, subsequent keys are tiebreakers.
        // Empty values are considered lower than all other values.
        if (input.len <= 1) {
            // Empty or single item - return as-is
            var out = ItemList.empty;
            for (input) |it| {
                try out.append(ctx.allocator, it);
            }
            return out;
        }

        // Create mutable copy of input for sorting
        var items = try ctx.allocator.alloc(item.Item, input.len);
        defer ctx.allocator.free(items);
        @memcpy(items, input);

        // Sort using insertion sort for stability and simplicity
        if (call.args.len == 0) {
            // No key selector - sort using default comparison
            var i: usize = 1;
            while (i < items.len) : (i += 1) {
                const key = items[i];
                var j: usize = i;
                while (j > 0) {
                    const cmp = compareItems(ctx, items[j - 1], key) catch return error.InvalidOperand;
                    if (cmp == null) return error.InvalidOperand;
                    if (cmp.? <= 0) break;
                    items[j] = items[j - 1];
                    j -= 1;
                }
                items[j] = key;
            }
        } else {
            // Key selectors provided - evaluate all keys for each item
            // key_values[item_idx][key_idx] = evaluated key value
            const num_keys = call.args.len;
            var all_key_values = try ctx.allocator.alloc([]ItemList, items.len);
            defer ctx.allocator.free(all_key_values);

            // Initialize and track what we've allocated
            var init_item_count: usize = 0;
            errdefer {
                for (all_key_values[0..init_item_count]) |key_row| {
                    for (key_row) |*kv| {
                        kv.deinit(ctx.allocator);
                    }
                    ctx.allocator.free(key_row);
                }
            }

            // Evaluate all keys for all items
            for (items, 0..) |it, item_idx| {
                var key_row = try ctx.allocator.alloc(ItemList, num_keys);
                for (key_row) |*kv| {
                    kv.* = ItemList.empty;
                }
                all_key_values[item_idx] = key_row;
                init_item_count = item_idx + 1;

                for (call.args, 0..) |arg, key_idx| {
                    key_row[key_idx] = try evalExpressionCtx(ctx, arg, it, env, item_idx);
                    // Key selector must return singleton or empty
                    if (key_row[key_idx].items.len > 1) return error.SingletonRequired;
                }
            }

            // Helper to compare two key values (handles empty)
            const compareKeyValues = struct {
                fn cmp(context: anytype, a: ItemList, b: ItemList) ?i32 {
                    if (a.items.len == 0 and b.items.len == 0) {
                        return 0; // Both empty, equal
                    } else if (a.items.len == 0) {
                        return -1; // a empty, b non-empty: a < b
                    } else if (b.items.len == 0) {
                        return 1; // a non-empty, b empty: a > b
                    } else {
                        return compareItems(context, a.items[0], b.items[0]) catch null;
                    }
                }
            }.cmp;

            // Get sort directions (null means all ascending)
            const directions = call.sort_directions;

            // Insertion sort using multi-key comparison
            var i: usize = 1;
            while (i < items.len) : (i += 1) {
                const key_item = items[i];
                const key_vals = all_key_values[i];
                var j: usize = i;
                while (j > 0) {
                    const prev_vals = all_key_values[j - 1];
                    // Compare using all keys in order
                    var cmp_result: i32 = 0;
                    for (0..num_keys) |k| {
                        var key_cmp = compareKeyValues(ctx, prev_vals[k], key_vals[k]);
                        if (key_cmp == null) return error.InvalidOperand;
                        // Apply direction: for desc, negate the comparison
                        if (directions != null and k < directions.?.len and directions.?[k] == .desc) {
                            key_cmp = -key_cmp.?;
                        }
                        if (key_cmp.? != 0) {
                            cmp_result = key_cmp.?;
                            break;
                        }
                    }
                    if (cmp_result <= 0) break;
                    items[j] = items[j - 1];
                    all_key_values[j] = all_key_values[j - 1];
                    j -= 1;
                }
                items[j] = key_item;
                all_key_values[j] = key_vals;
            }

            // Clean up all_key_values
            for (all_key_values) |key_row| {
                for (key_row) |*kv| {
                    kv.deinit(ctx.allocator);
                }
                ctx.allocator.free(key_row);
            }
        }

        // Build result
        var out = ItemList.empty;
        errdefer out.deinit(ctx.allocator);
        for (items) |it| {
            try out.append(ctx.allocator, it);
        }
        return out;
    }
    if (tag == .select) {
        if (call.args.len != 1) return error.InvalidFunction;
        var out = ItemList.empty;
        errdefer out.deinit(ctx.allocator);
        for (input, 0..) |it, idx| {
            const saved_root = ctx.root_item;
            const saved_index = ctx.current_index;
            ctx.root_item = it;
            ctx.current_index = idx;
            defer ctx.root_item = saved_root;
            defer ctx.current_index = saved_index;
            var projection = try evalExpressionCtx(ctx, call.args[0], it, env, idx);
            defer projection.deinit(ctx.allocator);
            if (projection.items.len == 0) continue;
            try out.appendSlice(ctx.allocator, projection.items);
        }
        return out;
    }
    if (tag == .where) {
        if (call.args.len != 1) return error.InvalidFunction;
        var out = ItemList.empty;
        errdefer out.deinit(ctx.allocator);
        for (input, 0..) |it, idx| {
            const saved_root = ctx.root_item;
            const saved_index = ctx.current_index;
            ctx.root_item = it;
            ctx.current_index = idx;
            defer ctx.root_item = saved_root;
            defer ctx.current_index = saved_index;
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
    if (tag == .all) {
        if (call.args.len != 1) return error.InvalidFunction;
        // Empty input collection returns true (vacuous truth)
        if (input.len == 0) {
            var out = ItemList.empty;
            try out.append(ctx.allocator, makeBoolItem(ctx, true));
            return out;
        }
        // Evaluate criteria for each element; all must return true
        for (input, 0..) |it, idx| {
            const saved_root = ctx.root_item;
            const saved_index = ctx.current_index;
            ctx.root_item = it;
            ctx.current_index = idx;
            defer ctx.root_item = saved_root;
            defer ctx.current_index = saved_index;
            var criteria = try evalExpressionCtx(ctx, call.args[0], it, env, idx);
            defer criteria.deinit(ctx.allocator);
            // If criteria returns empty, that's not true, so all() returns false
            if (criteria.items.len == 0) {
                var out = ItemList.empty;
                try out.append(ctx.allocator, makeBoolItem(ctx, false));
                return out;
            }
            // Criteria must be singleton boolean
            if (criteria.items.len != 1) return error.InvalidPredicate;
            const crit = criteria.items[0];
            if (!itemIsBool(ctx, crit)) return error.InvalidPredicate;
            // If any criteria evaluates to false, return false
            if (!itemBoolValue(ctx, crit)) {
                var out = ItemList.empty;
                try out.append(ctx.allocator, makeBoolItem(ctx, false));
                return out;
            }
        }
        // All criteria evaluated to true
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, true));
        return out;
    }
    if (tag == .repeat) {
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
    if (tag == .repeatAll) {
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
                const saved_root = ctx.root_item;
                const saved_index = ctx.current_index;
                ctx.root_item = it;
                ctx.current_index = idx;
                defer ctx.root_item = saved_root;
                defer ctx.current_index = saved_index;
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
    // children() returns the immediate child nodes of each input item
    if (tag == .children) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        errdefer out.deinit(ctx.allocator);
        const A = @TypeOf(ctx.adapter.*);

        for (input) |it| {
            // Only node_ref items can have children
            if (it.data_kind != .node_ref or it.node == null) continue;
            const ref = it.node.?;
            const k = A.kind(ctx.adapter, ref);

            switch (k) {
                .object => {
                    // Object children are the values of all fields
                    var iter = A.objectIter(ctx.adapter, ref);
                    while (iter.next()) |entry| {
                        // Schema-aware type lookup (mirrors applySegment)
                        var child_type_id: u32 = 0;
                        if (ctx.schema) |s| {
                            if (schema.isModelType(it.type_id)) {
                                if (s.childTypeForField(it.type_id, entry.key)) |tid| child_type_id = tid;
                            }
                        }
                        const child_ref = entry.value;
                        if (A.kind(ctx.adapter, child_ref) == .array) {
                            const len = A.arrayLen(ctx.adapter, child_ref);
                            for (0..len) |i| {
                                const elem_ref = A.arrayAt(ctx.adapter, child_ref, i);
                                const child_item = try itemFromNodeWithType(ctx, elem_ref, child_type_id);
                                try out.append(ctx.allocator, child_item);
                            }
                        } else {
                            const child_item = try itemFromNodeWithType(ctx, child_ref, child_type_id);
                            try out.append(ctx.allocator, child_item);
                        }
                    }
                },
                .array => {
                    // Array children are the array elements (inherit parent type)
                    const len = A.arrayLen(ctx.adapter, ref);
                    for (0..len) |i| {
                        const elem_ref = A.arrayAt(ctx.adapter, ref, i);
                        const child_item = try itemFromNodeWithType(ctx, elem_ref, it.type_id);
                        try out.append(ctx.allocator, child_item);
                    }
                },
                // Primitives have no children (extension/id/value are navigable
                // via property access but not included in children())
                .string, .number, .bool, .null => {},
            }
        }
        return out;
    }
    // descendants() returns all descendant nodes (children, grandchildren, etc.)
    // Spec: descendants() is defined as repeat(children())
    // Note: Arrays are transparent in FHIRPath - we traverse into array elements
    // but don't include the array itself as a descendant node.
    if (tag == .descendants) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        errdefer out.deinit(ctx.allocator);
        if (input.len == 0) return out;

        const A = @TypeOf(ctx.adapter.*);

        // BFS queue for traversal
        var queue = ItemList.empty;
        errdefer queue.deinit(ctx.allocator);
        try queue.appendSlice(ctx.allocator, input);

        var idx: usize = 0;
        while (idx < queue.items.len) : (idx += 1) {
            const it = queue.items[idx];
            // Only node_ref items can have children
            if (it.data_kind != .node_ref or it.node == null) continue;
            const ref = it.node.?;
            const k = A.kind(ctx.adapter, ref);

            switch (k) {
                .object => {
                    var iter = A.objectIter(ctx.adapter, ref);
                    while (iter.next()) |entry| {
                        // Schema-aware type lookup (mirrors applySegment)
                        var child_type_id: u32 = 0;
                        if (ctx.schema) |s| {
                            if (schema.isModelType(it.type_id)) {
                                if (s.childTypeForField(it.type_id, entry.key)) |tid| child_type_id = tid;
                            }
                        }
                        const child_ref = entry.value;
                        const child_kind = A.kind(ctx.adapter, child_ref);
                        if (child_kind == .array) {
                            // Arrays are transparent - include elements directly
                            const arr_len = A.arrayLen(ctx.adapter, child_ref);
                            for (0..arr_len) |i| {
                                const elem_ref = A.arrayAt(ctx.adapter, child_ref, i);
                                const elem_item = try itemFromNodeWithType(ctx, elem_ref, child_type_id);
                                try out.append(ctx.allocator, elem_item);
                                try queue.append(ctx.allocator, elem_item);
                            }
                        } else {
                            const child_item = try itemFromNodeWithType(ctx, child_ref, child_type_id);
                            try out.append(ctx.allocator, child_item);
                            try queue.append(ctx.allocator, child_item);
                        }
                    }
                },
                .array => {
                    // Arrays are transparent - include elements without including the array
                    const len = A.arrayLen(ctx.adapter, ref);
                    for (0..len) |i| {
                        const elem_ref = A.arrayAt(ctx.adapter, ref, i);
                        const child_item = try itemFromNodeWithType(ctx, elem_ref, it.type_id);
                        try out.append(ctx.allocator, child_item);
                        try queue.append(ctx.allocator, child_item);
                    }
                },
                .string, .number, .bool, .null => {},
            }
        }

        queue.deinit(ctx.allocator);
        return out;
    }
    if (tag == .sum) {
        if (call.args.len != 0) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;

        const first_val = itemToValue(ctx, input[0]);
        const first_kind = sumKindForValue(first_val) orelse return error.InvalidOperand;

        var out = ItemList.empty;
        switch (first_kind) {
            .integer => {
                var total: i64 = 0;
                for (input) |it| {
                    const val = itemToValue(ctx, it);
                    if (val != .integer) return error.InvalidOperand;
                    total +%= val.integer;
                }
                try out.append(ctx.allocator, makeIntegerItem(ctx, total));
                return out;
            },
            .long => {
                var total: i64 = 0;
                for (input) |it| {
                    const val = itemToValue(ctx, it);
                    if (val != .long) return error.InvalidOperand;
                    total +%= val.long;
                }
                try out.append(ctx.allocator, makeLongItemFromValue(ctx, total));
                return out;
            },
            .decimal => {
                var max_scale: u32 = 0;
                for (input) |it| {
                    const val = itemToValue(ctx, it);
                    if (val != .decimal) return error.InvalidOperand;
                    const scale = decimalScale(val.decimal) orelse return error.InvalidOperand;
                    if (scale > max_scale) max_scale = scale;
                }

                var total: i128 = 0;
                for (input) |it| {
                    const val = itemToValue(ctx, it);
                    const text = val.decimal;
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
                    const val = itemToValue(ctx, it);
                    if (val != .quantity) return error.InvalidOperand;
                    const q = val.quantity;
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
                    const val = itemToValue(ctx, it);
                    const q = val.quantity;
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
    if (tag == .avg) {
        if (call.args.len != 0) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;

        const first_val = itemToValue(ctx, input[0]);
        const first_kind = sumKindForValue(first_val) orelse return error.InvalidOperand;

        var out = ItemList.empty;
        const count: f64 = @floatFromInt(input.len);

        switch (first_kind) {
            .integer => {
                var total: f64 = 0;
                for (input) |it| {
                    const val = itemToValue(ctx, it);
                    if (val != .integer) return error.InvalidOperand;
                    total += @floatFromInt(val.integer);
                }
                try out.append(ctx.allocator, try makeDecimalItem(ctx, total / count));
                return out;
            },
            .long => {
                var total: f64 = 0;
                for (input) |it| {
                    const val = itemToValue(ctx, it);
                    if (val != .long) return error.InvalidOperand;
                    total += @floatFromInt(val.long);
                }
                try out.append(ctx.allocator, try makeDecimalItem(ctx, total / count));
                return out;
            },
            .decimal => {
                var total: f64 = 0;
                for (input) |it| {
                    const val = itemToValue(ctx, it);
                    if (val != .decimal) return error.InvalidOperand;
                    const parsed = std.fmt.parseFloat(f64, val.decimal) catch return error.InvalidOperand;
                    total += parsed;
                }
                try out.append(ctx.allocator, try makeDecimalItem(ctx, total / count));
                return out;
            },
            .quantity => {
                var unit: ?[]const u8 = null;
                var total: f64 = 0;
                for (input) |it| {
                    const val = itemToValue(ctx, it);
                    if (val != .quantity) return error.InvalidOperand;
                    const q = val.quantity;
                    if (unit == null) {
                        unit = q.unit;
                    } else if (!std.mem.eql(u8, unit.?, q.unit)) {
                        return error.InvalidOperand;
                    }
                    const parsed = std.fmt.parseFloat(f64, q.value) catch return error.InvalidOperand;
                    total += parsed;
                }

                const avg_val = total / count;
                var buf: [64]u8 = undefined;
                const avg_str = formatDecimal(&buf, avg_val);
                const owned = try ctx.allocator.dupe(u8, avg_str);
                try out.append(ctx.allocator, makeQuantityItem(ctx, owned, unit.?));
                return out;
            },
        }
    }
    if (tag == .min or tag == .max) {
        if (call.args.len != 0) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;

        const is_max = tag == .max;
        var best_idx: usize = 0;
        var best = input[0];

        for (input[1..], 1..) |candidate, idx| {
            const cmp = (compareItems(ctx, candidate, best) catch return error.InvalidOperand) orelse return error.InvalidOperand;
            if ((is_max and cmp > 0) or (!is_max and cmp < 0)) {
                best = candidate;
                best_idx = idx;
            }
        }

        var out = ItemList.empty;
        try out.append(ctx.allocator, input[best_idx]);
        return out;
    }
    if (tag == .aggregate) {
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
    if (tag == .single) {
        var out = ItemList.empty;
        if (input.len == 1) {
            try out.append(ctx.allocator, input[0]);
            return out;
        }
        if (input.len == 0) return out;
        return error.SingletonRequired;
    }
    // Reflection function: type() returns TypeInfo for each element
    if (tag == .@"type") {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        for (input) |it| {
            const info = getTypeInfoForItem(ctx, it);
            try out.append(ctx.allocator, makeTypeInfoItem(ctx, info.namespace, info.name));
        }
        return out;
    }
    if (tag == .first) {
        var out = ItemList.empty;
        if (input.len == 0) return out;
        try out.append(ctx.allocator, input[0]);
        return out;
    }
    if (tag == .last) {
        var out = ItemList.empty;
        if (input.len == 0) return out;
        try out.append(ctx.allocator, input[input.len - 1]);
        return out;
    }
    if (tag == .tail) {
        var out = ItemList.empty;
        if (input.len <= 1) return out;
        try out.appendSlice(ctx.allocator, input[1..]);
        return out;
    }
    if (tag == .skip) {
        const num = try parseIntegerArg(call);
        if (num <= 0) return sliceItems(ctx, input);
        const len_i64: i64 = @intCast(input.len);
        if (num >= len_i64) return ItemList.empty;
        const offset: usize = @intCast(num);
        return sliceItems(ctx, input[offset..]);
    }
    if (tag == .take) {
        const num = try parseIntegerArg(call);
        if (num <= 0) return ItemList.empty;
        const len_i64: i64 = @intCast(input.len);
        if (num >= len_i64) return sliceItems(ctx, input);
        const count: usize = @intCast(num);
        return sliceItems(ctx, input[0..count]);
    }
    // String matching functions: startsWith, endsWith, contains
    if (tag == .startsWith) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty; // empty input yields empty
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const prefix = try evalStringArg(ctx, call.args[0], env);
        if (prefix == null) return ItemList.empty; // empty argument yields empty
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, std.mem.startsWith(u8, str, prefix.?)));
        return out;
    }
    if (tag == .endsWith) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const suffix = try evalStringArg(ctx, call.args[0], env);
        if (suffix == null) return ItemList.empty;
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, std.mem.endsWith(u8, str, suffix.?)));
        return out;
    }
    if (tag == .contains) {
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
    if (tag == .substring) {
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
    if (tag == .abs) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalAbs(ctx, input[0]);
    }
    if (tag == .ceiling) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalCeiling(ctx, input[0]);
    }
    if (tag == .floor) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalFloor(ctx, input[0]);
    }
    if (tag == .truncate) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalTruncate(ctx, input[0]);
    }
    if (tag == .round) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const precision: i64 = if (call.args.len > 0) (try parseIntegerArg(call)) else 0;
        return evalRound(ctx, input[0], precision);
    }
    if (tag == .precision) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        if (val == .decimal) {
            const scale = decimalScale(val.decimal) orelse return out;
            try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(scale)));
            return out;
        }
        if (val == .date) {
            const prec = datePrecisionDigits(val.date) orelse return out;
            try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(prec)));
            return out;
        }
        if (val == .dateTime) {
            const prec = dateTimePrecisionDigits(val.dateTime) orelse return out;
            try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(prec)));
            return out;
        }
        if (val == .time) {
            const prec = timePrecisionDigits(val.time) orelse return out;
            try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(prec)));
            return out;
        }
        return out;
    }
    if (tag == .lowBoundary or tag == .highBoundary) {
        if (call.args.len > 1) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;

        const it = input[0];
        const val = itemToValue(ctx, it);
        const kind: BoundaryKind = if (tag == .lowBoundary) .low else .high;

        var precision_opt: ?i64 = null;
        if (call.args.len == 1) {
            precision_opt = try evalIntegerArg(ctx, call.args[0], env);
            if (precision_opt == null) return out;
        }
        if (precision_opt != null and precision_opt.? < 0) return out;

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

        if (val == .decimal or val == .integer) {
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
            } else {
                scale = decimalScale(val.decimal) orelse return out;
                base_scaled = parseDecimalScaled(val.decimal, scale) orelse return out;
            }

            const boundary_text = (try boundaryFromScaled(ctx.allocator, base_scaled, scale, precision, kind)) orelse return out;
            try out.append(ctx.allocator, makeDecimalItemText(ctx, boundary_text));
            return out;
        }

        if (val == .date) {
            const precision: u32 = if (precision_opt) |p| blk: {
                if (p > std.math.maxInt(u32)) return out;
                break :blk @intCast(p);
            } else DateMaxPrecision;
            if (precision > DateMaxPrecision) return out;
            const boundary_text = (try buildDateBoundary(ctx.allocator, val.date, precision, kind)) orelse return out;
            try out.append(ctx.allocator, makeDateItem(ctx, boundary_text));
            return out;
        }

        if (val == .dateTime) {
            const precision: u32 = if (precision_opt) |p| blk: {
                if (p > std.math.maxInt(u32)) return out;
                break :blk @intCast(p);
            } else DateTimeMaxPrecision;
            if (precision > DateTimeMaxPrecision) return out;
            const boundary_text = (try buildDateTimeBoundary(ctx.allocator, val.dateTime, precision, kind)) orelse return out;
            try out.append(ctx.allocator, makeDateTimeItem(ctx, boundary_text));
            return out;
        }

        if (val == .time) {
            const precision: u32 = if (precision_opt) |p| blk: {
                if (p > std.math.maxInt(u32)) return out;
                break :blk @intCast(p);
            } else TimeMaxPrecision;
            if (precision > TimeMaxPrecision) return out;
            const boundary_text = (try buildTimeBoundary(ctx.allocator, val.time, precision, kind)) orelse return out;
            try out.append(ctx.allocator, makeTimeItem(ctx, boundary_text));
            return out;
        }

        return out;
    }

    // Date/DateTime/Time component extraction functions (STU)
    if (tag == .yearOf) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        var text: ?[]const u8 = null;
        if (val == .date) text = val.date;
        if (val == .dateTime) text = val.dateTime;
        if (text == null) return out;

        const normalized = stripAtPrefix(text.?);
        // Get date portion (before T if present)
        const date_part = if (std.mem.indexOfScalar(u8, normalized, 'T')) |t_idx| normalized[0..t_idx] else normalized;
        const parts = parseDateParts(date_part) orelse return out;
        const year = std.fmt.parseInt(i64, parts.year, 10) catch return out;
        try out.append(ctx.allocator, makeIntegerItem(ctx, year));
        return out;
    }

    if (tag == .monthOf) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        var text: ?[]const u8 = null;
        if (val == .date) text = val.date;
        if (val == .dateTime) text = val.dateTime;
        if (text == null) return out;

        const normalized = stripAtPrefix(text.?);
        const date_part = if (std.mem.indexOfScalar(u8, normalized, 'T')) |t_idx| normalized[0..t_idx] else normalized;
        const parts = parseDateParts(date_part) orelse return out;
        const month_str = parts.month orelse return out; // empty if not present
        const month = std.fmt.parseInt(i64, month_str, 10) catch return out;
        try out.append(ctx.allocator, makeIntegerItem(ctx, month));
        return out;
    }

    if (tag == .dayOf) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        var text: ?[]const u8 = null;
        if (val == .date) text = val.date;
        if (val == .dateTime) text = val.dateTime;
        if (text == null) return out;

        const normalized = stripAtPrefix(text.?);
        const date_part = if (std.mem.indexOfScalar(u8, normalized, 'T')) |t_idx| normalized[0..t_idx] else normalized;
        const parts = parseDateParts(date_part) orelse return out;
        const day_str = parts.day orelse return out; // empty if not present
        const day = std.fmt.parseInt(i64, day_str, 10) catch return out;
        try out.append(ctx.allocator, makeIntegerItem(ctx, day));
        return out;
    }

    if (tag == .hourOf) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        var time_text: ?[]const u8 = null;
        if (val == .time) {
            time_text = val.time;
        } else if (val == .dateTime) {
            const normalized = stripAtPrefix(val.dateTime);
            if (std.mem.indexOfScalar(u8, normalized, 'T')) |t_idx| {
                time_text = normalized[t_idx + 1 ..];
            }
        }
        if (time_text == null) return out;

        const stripped = stripTimePrefix(time_text.?);
        if (stripped.len == 0) return out; // partial datetime with no time components
        const parts = parseTimePartsPartial(stripped) orelse return out;
        const hour = std.fmt.parseInt(i64, parts.hour, 10) catch return out;
        try out.append(ctx.allocator, makeIntegerItem(ctx, hour));
        return out;
    }

    if (tag == .minuteOf) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        var time_text: ?[]const u8 = null;
        if (val == .time) {
            time_text = val.time;
        } else if (val == .dateTime) {
            const normalized = stripAtPrefix(val.dateTime);
            if (std.mem.indexOfScalar(u8, normalized, 'T')) |t_idx| {
                time_text = normalized[t_idx + 1 ..];
            }
        }
        if (time_text == null) return out;

        const stripped = stripTimePrefix(time_text.?);
        if (stripped.len == 0) return out;
        const parts = parseTimePartsPartial(stripped) orelse return out;
        const minute_str = parts.minute orelse return out; // empty if not present
        const minute = std.fmt.parseInt(i64, minute_str, 10) catch return out;
        try out.append(ctx.allocator, makeIntegerItem(ctx, minute));
        return out;
    }

    if (tag == .secondOf) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        var time_text: ?[]const u8 = null;
        if (val == .time) {
            time_text = val.time;
        } else if (val == .dateTime) {
            const normalized = stripAtPrefix(val.dateTime);
            if (std.mem.indexOfScalar(u8, normalized, 'T')) |t_idx| {
                time_text = normalized[t_idx + 1 ..];
            }
        }
        if (time_text == null) return out;

        const stripped = stripTimePrefix(time_text.?);
        if (stripped.len == 0) return out;
        const parts = parseTimePartsPartial(stripped) orelse return out;
        const sec_str = parts.second orelse return out; // empty if not present
        // Second may have fractional part, we want just the integer portion
        const sec_parts = splitSecond(sec_str);
        const second = std.fmt.parseInt(i64, sec_parts.whole, 10) catch return out;
        try out.append(ctx.allocator, makeIntegerItem(ctx, second));
        return out;
    }

    if (tag == .millisecondOf) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        var time_text: ?[]const u8 = null;
        if (val == .time) {
            time_text = val.time;
        } else if (val == .dateTime) {
            const normalized = stripAtPrefix(val.dateTime);
            if (std.mem.indexOfScalar(u8, normalized, 'T')) |t_idx| {
                time_text = normalized[t_idx + 1 ..];
            }
        }
        if (time_text == null) return out;

        const stripped = stripTimePrefix(time_text.?);
        if (stripped.len == 0) return out;
        const parts = parseTimePartsPartial(stripped) orelse return out;
        const sec_str = parts.second orelse return out;
        const sec_parts = splitSecond(sec_str);
        const frac = sec_parts.frac orelse return out; // empty if no milliseconds
        // Parse the fractional part as milliseconds (pad or truncate to 3 digits)
        var ms_str: [3]u8 = undefined;
        var i: usize = 0;
        while (i < 3) : (i += 1) {
            ms_str[i] = if (i < frac.len) frac[i] else '0';
        }
        const ms = std.fmt.parseInt(i64, &ms_str, 10) catch return out;
        try out.append(ctx.allocator, makeIntegerItem(ctx, ms));
        return out;
    }

    if (tag == .timezoneOffsetOf) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        var text: ?[]const u8 = null;
        if (val == .dateTime) text = val.dateTime;
        if (text == null) return out;

        const normalized = stripAtPrefix(text.?);
        // Find the time portion
        const t_idx = std.mem.indexOfScalar(u8, normalized, 'T') orelse return out;
        const time_part = normalized[t_idx + 1 ..];

        // Find timezone: look for Z, +, or - (but - in a position that indicates timezone)
        var tz_start: ?usize = null;
        var idx: usize = 0;
        while (idx < time_part.len) : (idx += 1) {
            const c = time_part[idx];
            if (c == 'Z') {
                tz_start = idx;
                break;
            }
            if (c == '+') {
                tz_start = idx;
                break;
            }
            // For '-', we need to check if it's part of a time component (e.g., in seconds?) or timezone
            // Timezone '-' comes after the full time, so it shouldn't be at position 0 or 1
            if (c == '-' and idx >= 2) {
                // Could be timezone if we've passed hour:minute or hour:minute:second
                tz_start = idx;
                break;
            }
        }
        if (tz_start == null) return out; // no timezone present

        const tz_str = time_part[tz_start.?..];
        if (tz_str.len == 0) return out;

        if (tz_str[0] == 'Z') {
            try out.append(ctx.allocator, makeDecimalItemText(ctx, "0.0"));
            return out;
        }

        // Parse +HH:MM or -HH:MM
        if (tz_str.len < 6) return out; // need at least +HH:MM
        const sign: f64 = if (tz_str[0] == '-') -1.0 else 1.0;
        const hour_str = tz_str[1..3];
        const minute_str = if (tz_str.len >= 6 and tz_str[3] == ':') tz_str[4..6] else return out;

        const hour = std.fmt.parseInt(i32, hour_str, 10) catch return out;
        const minute = std.fmt.parseInt(i32, minute_str, 10) catch return out;

        const offset: f64 = sign * (@as(f64, @floatFromInt(hour)) + @as(f64, @floatFromInt(minute)) / 60.0);
        try out.append(ctx.allocator, try makeDecimalItem(ctx, offset));
        return out;
    }

    if (tag == .dateOf) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        if (val == .date) {
            // Date returns itself
            try out.append(ctx.allocator, it);
            return out;
        }

        var text: ?[]const u8 = null;
        if (val == .dateTime) text = val.dateTime;
        if (text == null) return out;

        const normalized = stripAtPrefix(text.?);
        // Extract date portion (before T if present)
        const date_part = if (std.mem.indexOfScalar(u8, normalized, 'T')) |t_idx| normalized[0..t_idx] else normalized;
        // Verify it parses as a valid date
        _ = parseDateParts(date_part) orelse return out;
        const owned = try ctx.allocator.dupe(u8, date_part);
        try out.append(ctx.allocator, makeDateItem(ctx, owned));
        return out;
    }

    if (tag == .timeOf) {
        if (call.args.len != 0) return error.InvalidFunction;
        var out = ItemList.empty;
        if (input.len == 0) return out;
        if (input.len != 1) return error.SingletonRequired;
        const it = input[0];
        const val = itemToValue(ctx, it);

        var text: ?[]const u8 = null;
        if (val == .dateTime) text = val.dateTime;
        if (text == null) return out;

        const normalized = stripAtPrefix(text.?);
        // Must have T for time component
        const t_idx = std.mem.indexOfScalar(u8, normalized, 'T') orelse return out;
        var time_part = normalized[t_idx + 1 ..];
        if (time_part.len == 0) return out; // partial datetime with no time

        // Strip timezone for the result
        var tz_start: ?usize = null;
        var idx: usize = 0;
        while (idx < time_part.len) : (idx += 1) {
            const c = time_part[idx];
            if (c == 'Z' or c == '+') {
                tz_start = idx;
                break;
            }
            if (c == '-' and idx >= 2) {
                tz_start = idx;
                break;
            }
        }
        if (tz_start) |tz| {
            time_part = time_part[0..tz];
        }

        // Verify it parses as valid time
        const parts = parseTimePartsPartial(time_part) orelse return out;
        if (parts.hour.len == 0) return out;

        const owned = try ctx.allocator.dupe(u8, time_part);
        try out.append(ctx.allocator, makeTimeItem(ctx, owned));
        return out;
    }

    if (tag == .exp) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalExp(ctx, input[0]);
    }
    if (tag == .ln) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalLn(ctx, input[0]);
    }
    if (tag == .log) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const base_opt = try evalDecimalArg(ctx, call.args[0], env);
        if (base_opt == null) return ItemList.empty; // empty base yields empty
        return evalLog(ctx, input[0], base_opt.?);
    }
    if (tag == .sqrt) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        return evalSqrt(ctx, input[0]);
    }
    if (tag == .power) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const exp_opt = try evalDecimalArg(ctx, call.args[0], env);
        if (exp_opt == null) return ItemList.empty; // empty exponent yields empty
        return evalPower(ctx, input[0], exp_opt.?);
    }
    // iif(criterion, true-result [, otherwise-result])
    if (tag == .iif) {
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
        var criterion_result = try evalExpressionCtx(ctx, call.args[0], context_item, env, ctx.current_index);
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
            return evalExpressionCtx(ctx, call.args[1], context_item, env, ctx.current_index);
        } else {
            // Evaluate and return otherwise-result (or empty if not provided)
            if (call.args.len == 3) {
                return evalExpressionCtx(ctx, call.args[2], context_item, env, ctx.current_index);
            } else {
                return ItemList.empty;
            }
        }
    }
    // coalesce(arg1, arg2, ...) - returns the first non-empty argument
    // Note: coalesce is always a standalone function call, never a method.
    // The "input" here is the evaluation context (e.g., the document root),
    // and all coalesce arguments are in call.args.
    if (tag == .coalesce) {
        if (call.args.len < 1) return error.InvalidFunction;
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
        // Short-circuit evaluation: return the first non-empty argument
        for (call.args) |arg| {
            var result = try evalExpressionCtx(ctx, arg, context_item, env, null);
            if (result.items.len > 0) {
                // Found a non-empty result, return it
                return result;
            }
            // Empty result, continue to next argument
            result.deinit(ctx.allocator);
        }
        // All arguments were empty
        return ItemList.empty;
    }
    // String manipulation functions
    if (tag == .length) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeIntegerItem(ctx, @intCast(str.len)));
        return out;
    }
    if (tag == .indexOf) {
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
    if (tag == .lastIndexOf) {
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
    if (tag == .upper) {
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
    if (tag == .lower) {
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
    if (tag == .replace) {
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
    // Regex matching functions: matches, matchesFull, replaceMatches
    if (tag == .matches) {
        if (call.args.len < 1 or call.args.len > 2) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const pattern = try evalStringArg(ctx, call.args[0], env);
        if (pattern == null) return ItemList.empty;
        const flags_str = if (call.args.len == 2) try evalStringArg(ctx, call.args[1], env) else null;
        const flags = regex.parseFlags(flags_str);
        const re = regex.Regex.init(pattern.?, flags);
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, re.matches(str)));
        return out;
    }
    if (tag == .matchesFull) {
        if (call.args.len < 1 or call.args.len > 2) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const pattern = try evalStringArg(ctx, call.args[0], env);
        if (pattern == null) return ItemList.empty;
        const flags_str = if (call.args.len == 2) try evalStringArg(ctx, call.args[1], env) else null;
        const flags = regex.parseFlags(flags_str);
        const re = regex.Regex.init(pattern.?, flags);
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeBoolItem(ctx, re.matchesFull(str)));
        return out;
    }
    if (tag == .replaceMatches) {
        if (call.args.len < 2 or call.args.len > 3) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const pattern = try evalStringArg(ctx, call.args[0], env);
        const substitution = try evalStringArg(ctx, call.args[1], env);
        if (pattern == null or substitution == null) return ItemList.empty;
        const flags_str = if (call.args.len == 3) try evalStringArg(ctx, call.args[2], env) else null;
        const flags = regex.parseFlags(flags_str);
        const re = regex.Regex.init(pattern.?, flags);
        const result = try re.replaceAll(ctx.allocator, str, substitution.?);
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeStringItem(ctx, result));
        return out;
    }
    if (tag == .trim) {
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const trimmed = std.mem.trim(u8, str, " \t\n\r");
        var out = ItemList.empty;
        try out.append(ctx.allocator, makeStringItem(ctx, trimmed));
        return out;
    }
    if (tag == .toChars) {
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
    if (tag == .split) {
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
    if (tag == .join) {
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
    if (tag == .@"union") {
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
    if (tag == .combine) {
        if (call.args.len < 1 or call.args.len > 2) return error.InvalidFunction;
        var other = try evalCollectionArg(ctx, call.args[0], env);
        defer other.deinit(ctx.allocator);
        // Merge without deduplication
        var out = ItemList.empty;
        try out.appendSlice(ctx.allocator, input);
        try out.appendSlice(ctx.allocator, other.items);
        return out;
    }
    if (tag == .intersect) {
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
    if (tag == .subsetOf) {
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
    if (tag == .supersetOf) {
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
    if (tag == .exclude) {
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
    // defineVariable(name [, expr]) - defines a variable accessible in subsequent expressions
    // If expr is provided, variable holds the evaluated result
    // If expr is omitted, variable holds the input collection
    // Returns the input collection unchanged (pass-through semantics)
    if (tag == .defineVariable) {
        if (call.args.len < 1 or call.args.len > 2) return error.InvalidFunction;

        // First arg must be a string literal (variable name)
        const name = switch (call.args[0]) {
            .Literal => |lit| switch (lit) {
                .String => |s| s,
                else => return error.InvalidFunction,
            },
            else => return error.InvalidFunction,
        };

        // Determine the value: either from expr arg or from input collection
        const var_value: []const item.Item = if (call.args.len == 2) blk: {
            // Evaluate the expression to get the variable value
            // Special handling: if the expression is a Path with root=This, evaluate it
            // using the input collection as the initial context (for functions like sum())
            var expr_result = try evalExprOnCollection(ctx, call.args[1], input, env);
            defer expr_result.deinit(ctx.allocator);
            // Copy items to arena (they may be freed after defer)
            const items_copy = try ctx.allocator.alloc(item.Item, expr_result.items.len);
            @memcpy(items_copy, expr_result.items);
            break :blk items_copy;
        } else blk: {
            // Use input collection as the value
            const items_copy = try ctx.allocator.alloc(item.Item, input.len);
            @memcpy(items_copy, input);
            break :blk items_copy;
        };

        // Add to environment (requires mutable env - evalInvoke creates local env for this)
        if (env) |e| {
            try e.map.put(name, var_value);
        }

        // Return input unchanged (pass-through)
        var out = ItemList.empty;
        try out.appendSlice(ctx.allocator, input);
        return out;
    }
    // encode(format: String) - Encodes string in hex, base64, or urlbase64 format
    if (tag == .encode) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const format = try evalStringArg(ctx, call.args[0], env);
        if (format == null) return ItemList.empty;
        var out = ItemList.empty;
        if (std.mem.eql(u8, format.?, "hex")) {
            // Hex encoding: each byte becomes two hex chars (lowercase)
            if (str.len == 0) {
                try out.append(ctx.allocator, makeStringItem(ctx, ""));
                return out;
            }
            const hex_str = try ctx.allocator.alloc(u8, str.len * 2);
            const hex_chars = "0123456789abcdef";
            for (str, 0..) |c, i| {
                hex_str[i * 2] = hex_chars[c >> 4];
                hex_str[i * 2 + 1] = hex_chars[c & 0x0f];
            }
            try out.append(ctx.allocator, makeStringItem(ctx, hex_str));
        } else if (std.mem.eql(u8, format.?, "base64") or std.mem.eql(u8, format.?, "urlbase64")) {
            // Base64 encoding
            const is_url = std.mem.eql(u8, format.?, "urlbase64");
            const alphabet = if (is_url) std.base64.url_safe.Encoder.alphabet_chars else std.base64.standard.Encoder.alphabet_chars;
            const pad_char = '=';
            if (str.len == 0) {
                try out.append(ctx.allocator, makeStringItem(ctx, ""));
                return out;
            }
            // Calculate output size
            const encoded_len = (str.len + 2) / 3 * 4;
            const encoded = try ctx.allocator.alloc(u8, encoded_len);
            var i: usize = 0;
            var j: usize = 0;
            while (i + 3 <= str.len) : (i += 3) {
                const b0 = str[i];
                const b1 = str[i + 1];
                const b2 = str[i + 2];
                encoded[j] = alphabet[b0 >> 2];
                encoded[j + 1] = alphabet[((b0 & 0x03) << 4) | (b1 >> 4)];
                encoded[j + 2] = alphabet[((b1 & 0x0f) << 2) | (b2 >> 6)];
                encoded[j + 3] = alphabet[b2 & 0x3f];
                j += 4;
            }
            // Handle remaining bytes
            const remaining = str.len - i;
            if (remaining == 1) {
                const b0 = str[i];
                encoded[j] = alphabet[b0 >> 2];
                encoded[j + 1] = alphabet[(b0 & 0x03) << 4];
                encoded[j + 2] = pad_char;
                encoded[j + 3] = pad_char;
            } else if (remaining == 2) {
                const b0 = str[i];
                const b1 = str[i + 1];
                encoded[j] = alphabet[b0 >> 2];
                encoded[j + 1] = alphabet[((b0 & 0x03) << 4) | (b1 >> 4)];
                encoded[j + 2] = alphabet[(b1 & 0x0f) << 2];
                encoded[j + 3] = pad_char;
            }
            try out.append(ctx.allocator, makeStringItem(ctx, encoded));
        } else {
            return ItemList.empty; // Unknown format
        }
        return out;
    }
    // decode(format: String) - Decodes string from hex, base64, or urlbase64 format
    if (tag == .decode) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const format = try evalStringArg(ctx, call.args[0], env);
        if (format == null) return ItemList.empty;
        var out = ItemList.empty;
        if (std.mem.eql(u8, format.?, "hex")) {
            // Hex decoding
            if (str.len == 0) {
                try out.append(ctx.allocator, makeStringItem(ctx, ""));
                return out;
            }
            if (str.len % 2 != 0) return ItemList.empty; // Invalid hex
            const decoded = try ctx.allocator.alloc(u8, str.len / 2);
            var i: usize = 0;
            while (i < str.len) : (i += 2) {
                const hi = hexDigitValue(str[i]) orelse return ItemList.empty;
                const lo = hexDigitValue(str[i + 1]) orelse return ItemList.empty;
                decoded[i / 2] = (hi << 4) | lo;
            }
            try out.append(ctx.allocator, makeStringItem(ctx, decoded));
        } else if (std.mem.eql(u8, format.?, "base64") or std.mem.eql(u8, format.?, "urlbase64")) {
            // Base64 decoding
            const is_url = std.mem.eql(u8, format.?, "urlbase64");
            if (str.len == 0) {
                try out.append(ctx.allocator, makeStringItem(ctx, ""));
                return out;
            }
            // Calculate output size (accounting for padding)
            var padding: usize = 0;
            if (str.len > 0 and str[str.len - 1] == '=') padding += 1;
            if (str.len > 1 and str[str.len - 2] == '=') padding += 1;
            const decoded_len = str.len / 4 * 3 - padding;
            const decoded = try ctx.allocator.alloc(u8, decoded_len);
            var i: usize = 0;
            var j: usize = 0;
            while (i + 4 <= str.len) : (i += 4) {
                const c0 = base64CharValue(str[i], is_url) orelse return ItemList.empty;
                const c1 = base64CharValue(str[i + 1], is_url) orelse return ItemList.empty;
                const c2 = if (str[i + 2] == '=') @as(u8, 0) else (base64CharValue(str[i + 2], is_url) orelse return ItemList.empty);
                const c3 = if (str[i + 3] == '=') @as(u8, 0) else (base64CharValue(str[i + 3], is_url) orelse return ItemList.empty);
                if (j < decoded_len) decoded[j] = (c0 << 2) | (c1 >> 4);
                if (j + 1 < decoded_len) decoded[j + 1] = ((c1 & 0x0f) << 4) | (c2 >> 2);
                if (j + 2 < decoded_len) decoded[j + 2] = ((c2 & 0x03) << 6) | c3;
                j += 3;
            }
            try out.append(ctx.allocator, makeStringItem(ctx, decoded));
        } else {
            return ItemList.empty; // Unknown format
        }
        return out;
    }
    // escape(target: String) - Escapes string for html or json
    if (tag == .escape) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const target = try evalStringArg(ctx, call.args[0], env);
        if (target == null) return ItemList.empty;
        var out = ItemList.empty;
        if (std.mem.eql(u8, target.?, "html")) {
            // HTML escape: < > & "
            var result = std.ArrayList(u8).empty;
            defer result.deinit(ctx.allocator);
            for (str) |c| {
                switch (c) {
                    '<' => try result.appendSlice(ctx.allocator, "&lt;"),
                    '>' => try result.appendSlice(ctx.allocator, "&gt;"),
                    '&' => try result.appendSlice(ctx.allocator, "&amp;"),
                    '"' => try result.appendSlice(ctx.allocator, "&quot;"),
                    else => try result.append(ctx.allocator, c),
                }
            }
            const escaped = try result.toOwnedSlice(ctx.allocator);
            try out.append(ctx.allocator, makeStringItem(ctx, escaped));
        } else if (std.mem.eql(u8, target.?, "json")) {
            // JSON escape: " \ and control characters
            var result = std.ArrayList(u8).empty;
            defer result.deinit(ctx.allocator);
            for (str) |c| {
                switch (c) {
                    '"' => try result.appendSlice(ctx.allocator, "\\\""),
                    '\\' => try result.appendSlice(ctx.allocator, "\\\\"),
                    '\n' => try result.appendSlice(ctx.allocator, "\\n"),
                    '\r' => try result.appendSlice(ctx.allocator, "\\r"),
                    '\t' => try result.appendSlice(ctx.allocator, "\\t"),
                    else => {
                        if (c < 0x20) {
                            // Control character - use unicode escape
                            try result.appendSlice(ctx.allocator, "\\u00");
                            const hex_chars = "0123456789abcdef";
                            try result.append(ctx.allocator, hex_chars[c >> 4]);
                            try result.append(ctx.allocator, hex_chars[c & 0x0f]);
                        } else {
                            try result.append(ctx.allocator, c);
                        }
                    },
                }
            }
            const escaped = try result.toOwnedSlice(ctx.allocator);
            try out.append(ctx.allocator, makeStringItem(ctx, escaped));
        } else {
            return ItemList.empty; // Unknown target
        }
        return out;
    }
    // unescape(target: String) - Unescapes string from html or json
    if (tag == .unescape) {
        if (call.args.len != 1) return error.InvalidFunction;
        if (input.len == 0) return ItemList.empty;
        if (input.len > 1) return error.SingletonRequired;
        const str = itemStringValue(ctx, input[0]) orelse return error.InvalidFunction;
        const target = try evalStringArg(ctx, call.args[0], env);
        if (target == null) return ItemList.empty;
        var out = ItemList.empty;
        if (std.mem.eql(u8, target.?, "html")) {
            // HTML unescape: &lt; &gt; &amp; &quot;
            var result = std.ArrayList(u8).empty;
            defer result.deinit(ctx.allocator);
            var i: usize = 0;
            while (i < str.len) {
                if (str[i] == '&') {
                    if (i + 3 < str.len and std.mem.eql(u8, str[i .. i + 4], "&lt;")) {
                        try result.append(ctx.allocator, '<');
                        i += 4;
                    } else if (i + 3 < str.len and std.mem.eql(u8, str[i .. i + 4], "&gt;")) {
                        try result.append(ctx.allocator, '>');
                        i += 4;
                    } else if (i + 4 < str.len and std.mem.eql(u8, str[i .. i + 5], "&amp;")) {
                        try result.append(ctx.allocator, '&');
                        i += 5;
                    } else if (i + 5 < str.len and std.mem.eql(u8, str[i .. i + 6], "&quot;")) {
                        try result.append(ctx.allocator, '"');
                        i += 6;
                    } else {
                        try result.append(ctx.allocator, str[i]);
                        i += 1;
                    }
                } else {
                    try result.append(ctx.allocator, str[i]);
                    i += 1;
                }
            }
            const unescaped = try result.toOwnedSlice(ctx.allocator);
            try out.append(ctx.allocator, makeStringItem(ctx, unescaped));
        } else if (std.mem.eql(u8, target.?, "json")) {
            // JSON unescape: \" \\ \n \r \t \uXXXX
            var result = std.ArrayList(u8).empty;
            defer result.deinit(ctx.allocator);
            var i: usize = 0;
            while (i < str.len) {
                if (str[i] == '\\' and i + 1 < str.len) {
                    switch (str[i + 1]) {
                        '"' => {
                            try result.append(ctx.allocator, '"');
                            i += 2;
                        },
                        '\\' => {
                            try result.append(ctx.allocator, '\\');
                            i += 2;
                        },
                        'n' => {
                            try result.append(ctx.allocator, '\n');
                            i += 2;
                        },
                        'r' => {
                            try result.append(ctx.allocator, '\r');
                            i += 2;
                        },
                        't' => {
                            try result.append(ctx.allocator, '\t');
                            i += 2;
                        },
                        'u' => {
                            // Unicode escape \uXXXX
                            if (i + 5 < str.len) {
                                var code: u16 = 0;
                                var valid = true;
                                for (str[i + 2 .. i + 6]) |c| {
                                    if (hexDigitValue(c)) |v| {
                                        code = code * 16 + v;
                                    } else {
                                        valid = false;
                                        break;
                                    }
                                }
                                if (valid and code < 128) {
                                    try result.append(ctx.allocator, @intCast(code));
                                    i += 6;
                                } else {
                                    // Non-ASCII or invalid - pass through
                                    try result.append(ctx.allocator, str[i]);
                                    i += 1;
                                }
                            } else {
                                try result.append(ctx.allocator, str[i]);
                                i += 1;
                            }
                        },
                        else => {
                            try result.append(ctx.allocator, str[i]);
                            i += 1;
                        },
                    }
                } else {
                    try result.append(ctx.allocator, str[i]);
                    i += 1;
                }
            }
            const unescaped = try result.toOwnedSlice(ctx.allocator);
            try out.append(ctx.allocator, makeStringItem(ctx, unescaped));
        } else {
            return ItemList.empty; // Unknown target
        }
        return out;
    }
    // trace(name: String [, projection: Expression]) - debugging function that returns input unchanged
    // The name and optional projection are for logging; the function always returns its input.
    if (tag == .trace) {
        if (call.args.len < 1 or call.args.len > 2) return error.InvalidFunction;
        // Note: We don't actually log anything - that's implementation-defined.
        // The key behavior is that trace() returns its input unchanged.
        // If there's a projection (2nd arg), we'd evaluate it for logging but still return input.
        // Since we're not logging, we don't need to evaluate the projection.
        var out = ItemList.empty;
        try out.appendSlice(ctx.allocator, input);
        return out;
    }
    return error.InvalidFunction;
}

/// Evaluates an expression using a collection as the initial context.
/// For Path expressions with root=This, the input collection becomes the starting point.
/// This allows expressions like sum() to operate on the collection.
fn evalExprOnCollection(
    ctx: anytype,
    expr: ast.Expr,
    input: []const item.Item,
    env: ?*Env,
) EvalError!ItemList {
    switch (expr) {
        .Path => |path| {
            // If the path starts with This, use the input collection as the starting point
            if (path.root == .This) {
                var current = ItemList.empty;
                errdefer current.deinit(ctx.allocator);
                try current.appendSlice(ctx.allocator, input);

                // Apply the steps
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
                        },
                        .Index => |idx| {
                            var next = ItemList.empty;
                            errdefer next.deinit(ctx.allocator);
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
            // For other roots (Env, Literal, etc.), evaluate normally
            const root_item = if (input.len > 0) input[0] else item.Item{
                .data_kind = .none,
                .value_kind = .empty,
                .type_id = 0,
                .source_pos = 0,
                .source_end = 0,
                .node = null,
                .value = .{ .empty = {} },
            };
            return evalExpressionCtx(ctx, expr, root_item, env, null);
        },
        else => {
            // For non-Path expressions, evaluate with first input item as context
            const root_item = if (input.len > 0) input[0] else item.Item{
                .data_kind = .none,
                .value_kind = .empty,
                .type_id = 0,
                .source_pos = 0,
                .source_end = 0,
                .node = null,
                .value = .{ .empty = {} },
            };
            return evalExpressionCtx(ctx, expr, root_item, env, null);
        },
    }
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
    var result = try evalExpressionCtx(ctx, expr, ctx.root_item, env, null);
    defer result.deinit(ctx.allocator);
    if (result.items.len == 0) return null;
    // Multi-item is an error per spec: singleton evaluation of collections
    if (result.items.len > 1) return error.SingletonRequired;
    return itemStringValue(ctx, result.items[0]);
}

fn evalIntegerArg(ctx: anytype, expr: ast.Expr, env: ?*Env) EvalError!?i64 {
    // For integer literal, return directly
    if (integerLiteral(expr)) |i| return i;
    // Evaluate the expression and get integer value
    var result = try evalExpressionCtx(ctx, expr, ctx.root_item, env, null);
    defer result.deinit(ctx.allocator);
    if (result.items.len == 0) return null;
    // Multi-item is an error per spec: singleton evaluation of collections
    if (result.items.len > 1) return error.SingletonRequired;
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
    var result = try evalExpressionCtx(ctx, expr, ctx.root_item, env, null);
    defer result.deinit(ctx.allocator);
    if (result.items.len == 0) return null;
    // Multi-item is an error per spec: singleton evaluation of collections
    if (result.items.len > 1) return error.SingletonRequired;
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

fn hexDigitValue(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => null,
    };
}

fn base64CharValue(c: u8, is_url: bool) ?u8 {
    if (c >= 'A' and c <= 'Z') return c - 'A';
    if (c >= 'a' and c <= 'z') return c - 'a' + 26;
    if (c >= '0' and c <= '9') return c - '0' + 52;
    if (is_url) {
        if (c == '-') return 62;
        if (c == '_') return 63;
    } else {
        if (c == '+') return 62;
        if (c == '/') return 63;
    }
    return null;
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
    return itemsEqualTriState(ctx, a, b) orelse false;
}

/// Three-valued equality: true, false, or null (empty - precision mismatch).
/// Used by the = operator which needs to propagate empty for date/time precision differences.
fn itemsEqualTriState(ctx: anytype, a: item.Item, b: item.Item) ?bool {
    const va = itemToValue(ctx, a);
    const vb = itemToValue(ctx, b);

    // Check if either operand is Date or DateTime (by value kind)
    const a_is_date = va == .date;
    const a_is_datetime = va == .dateTime;
    const b_is_date = vb == .date;
    const b_is_datetime = vb == .dateTime;

    // Cross-type Date/DateTime equality (implicit conversion)
    // Note: equality also returns null for precision mismatch
    if ((a_is_date or a_is_datetime) and (b_is_date or b_is_datetime)) {
        const a_text = getDateTimeText(va);
        const b_text = getDateTimeText(vb);
        if (a_text != null and b_text != null) {
            const cmp = compareDateAndDateTime(a_text.?, a_is_datetime, b_text.?, b_is_datetime);
            if (cmp) |c| return c == 0;
            return null; // precision mismatch -> empty
        }
    }

    // Both value-backed items with same value kinds
    if (a.data_kind == .value and b.data_kind == .value) {
        return valueEqualTriState(a.value.?, b.value.?);
    }
    if (a.data_kind == .node_ref and b.data_kind == .node_ref and a.node != null and b.node != null) {
        return nodeEqual(ctx, a.node.?, b.node.?);
    }
    return valueEqualTriState(va, vb);
}

/// Extract date/time text from a Value
fn getDateTimeText(v: item.Value) ?[]const u8 {
    return switch (v) {
        .dateTime => |dt| dt,
        .date => |d| d,
        .string => |s| s,
        else => null,
    };
}

/// Three-valued value equality. Returns null for date/time precision mismatch.
fn valueEqualTriState(a: item.Value, b: item.Value) ?bool {
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
        .long => |v| return v == b.long,
        .decimal => |v| return compareDecimalStrings(v, b.decimal) == 0,
        .string => |v| return std.mem.eql(u8, v, b.string),
        .date => |v| {
            // Different precision -> empty (null)
            if (v.len != b.date.len) return null;
            return std.mem.eql(u8, v, b.date);
        },
        .time => |v| {
            // Per spec, seconds and milliseconds are single precision
            const a_prec = timePrecisionLevel(v);
            const b_prec = timePrecisionLevel(b.time);
            if (a_prec != b_prec) return null;
            return compareTimeStrings(v, b.time) == 0;
        },
        .dateTime => |v| {
            const a_prec = dateTimePrecisionLevel(v);
            const b_prec = dateTimePrecisionLevel(b.dateTime);
            if (a_prec != b_prec) return null;
            return compareDateTimeStrings(v, b.dateTime) == 0;
        },
        .quantity => |v| {
            return compareQuantitiesEqual(v, b.quantity);
        },
        .typeInfo => |v| {
            return std.mem.eql(u8, v.namespace, b.typeInfo.namespace) and
                std.mem.eql(u8, v.name, b.typeInfo.name);
        },
    }
}

fn valueToNumber(v: item.Value) ?f64 {
    return switch (v) {
        .integer => |i| @floatFromInt(i),
        .long => |i| @floatFromInt(i),
        .decimal => |s| std.fmt.parseFloat(f64, s) catch null,
        else => null,
    };
}

fn nodeEqual(ctx: anytype, a_ref: @TypeOf(ctx.adapter.*).NodeRef, b_ref: @TypeOf(ctx.adapter.*).NodeRef) bool {
    const A = @TypeOf(ctx.adapter.*);
    if (a_ref == b_ref) return true;
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
        .Long => |n| makeLongItem(ctx, n),
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
    // Determine the type_id. If an override is provided, use it, but also check if
    // the node has a resourceType field that indicates a more specific type.
    // This handles polymorphic fields like Bundle.entry.resource which are typed
    // as 'Resource' in the schema but have actual resource types at runtime.
    const type_id = blk: {
        if (type_id_override != 0) {
            // Check if this is a model type that could have a more specific runtime type
            if (schema.isModelType(type_id_override) and A.kind(ctx.adapter, node_ref) == .object) {
                if (resourceTypeName(ctx, node_ref)) |rt| {
                    if (ctx.schema) |s| {
                        if (s.typeIdByLocalName(rt)) |specific_tid| {
                            // Use the more specific type if it's a subtype of the override
                            if (s.isSubtype(specific_tid, type_id_override)) {
                                break :blk specific_tid;
                            }
                        }
                    }
                }
            }
            break :blk type_id_override;
        }
        break :blk typeIdForNode(ctx, node_ref);
    };
    var source_pos: u32 = 0;
    var source_end: u32 = 0;
    if (comptime node.hasSpanSupport(A)) {
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
        .node = node_ref,
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

fn makeBoolItem(_: anytype, v: bool) item.Item {
    const type_id = item.SystemTypeIds.boolean;
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

fn makeIntegerItem(_: anytype, v: i64) item.Item {
    const type_id = item.SystemTypeIds.integer;
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

fn makeLongItem(_: anytype, raw: []const u8) item.Item {
    const parsed = std.fmt.parseInt(i64, raw, 10) catch 0;
    return makeLongItemFromValue(.{}, parsed);
}

fn makeLongItemFromValue(_: anytype, v: i64) item.Item {
    const type_id = item.SystemTypeIds.long;
    return .{
        .data_kind = .value,
        .value_kind = .long,
        .type_id = type_id,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .long = v },
    };
}

fn makeNumberItem(_: anytype, raw: []const u8) item.Item {
    if (isInteger(raw)) {
        const parsed = std.fmt.parseInt(i64, raw, 10) catch 0;
        return makeIntegerItem(.{}, parsed);
    }
    const type_id = item.SystemTypeIds.decimal;
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

fn makeStringItem(_: anytype, s: []const u8) item.Item {
    const type_id = item.SystemTypeIds.string;
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

fn makeDateItem(_: anytype, s: []const u8) item.Item {
    const type_id = item.SystemTypeIds.date;
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

fn makeDateTimeItem(_: anytype, s: []const u8) item.Item {
    const type_id = item.SystemTypeIds.dateTime;
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

fn makeTimeItem(_: anytype, s: []const u8) item.Item {
    const type_id = item.SystemTypeIds.time;
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

fn makeQuantityItem(_: anytype, value: []const u8, unit: []const u8) item.Item {
    const type_id = item.SystemTypeIds.quantity;
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

fn makeTypeInfoItem(_: anytype, namespace: []const u8, name: []const u8) item.Item {
    return .{
        .data_kind = .value,
        .value_kind = .typeInfo,
        .type_id = item.SystemTypeIds.typeInfo,
        .source_pos = 0,
        .source_end = 0,
        .node = null,
        .value = .{ .typeInfo = .{ .namespace = namespace, .name = name } },
    };
}

/// Get namespace and name for an item's type.
/// Returns namespace and name strings.
fn getTypeInfoForItem(ctx: anytype, it: item.Item) struct { namespace: []const u8, name: []const u8 } {
    // Check if we have an explicit type_id (from model/schema navigation)
    // This takes precedence over inferred types
    if (it.type_id != 0) {
        // For model types, use schema.typeName; for others use types.name
        const type_name: []const u8 = if (schema.isModelType(it.type_id))
            if (ctx.schema) |s| s.typeName(it.type_id) else ""
        else
            schema.systemTypeName(it.type_id);

        if (type_name.len > 0) {
            // Parse "Namespace.Name" format
            if (std.mem.indexOf(u8, type_name, ".")) |dot_pos| {
                return .{
                    .namespace = type_name[0..dot_pos],
                    .name = type_name[dot_pos + 1 ..],
                };
            }
            // No namespace prefix - could be a bare System type
            return .{ .namespace = "System", .name = type_name };
        }
    }

    // For value items without type_id, use value_kind to determine type
    if (it.data_kind == .value and it.value != null) {
        return switch (it.value.?) {
            .empty => .{ .namespace = "System", .name = "Any" },
            .boolean => .{ .namespace = "System", .name = "Boolean" },
            .integer => .{ .namespace = "System", .name = "Integer" },
            .long => .{ .namespace = "System", .name = "Long" },
            .decimal => .{ .namespace = "System", .name = "Decimal" },
            .string => .{ .namespace = "System", .name = "String" },
            .date => .{ .namespace = "System", .name = "Date" },
            .time => .{ .namespace = "System", .name = "Time" },
            .dateTime => .{ .namespace = "System", .name = "DateTime" },
            .quantity => .{ .namespace = "System", .name = "Quantity" },
            .typeInfo => .{ .namespace = "System", .name = "TypeInfo" },
        };
    }

    // For node-backed items without type_id, infer from JSON type
    if (it.data_kind == .node_ref and it.node != null) {
        const A = @TypeOf(ctx.adapter.*);
        const ref = it.node.?;
        return switch (A.kind(ctx.adapter, ref)) {
            .bool => .{ .namespace = "System", .name = "Boolean" },
            .number => blk: {
                // Check if integer or decimal
                const text = A.numberText(ctx.adapter, ref);
                if (isInteger(text)) {
                    break :blk .{ .namespace = "System", .name = "Integer" };
                }
                break :blk .{ .namespace = "System", .name = "Decimal" };
            },
            .string => .{ .namespace = "System", .name = "String" },
            .object, .array => .{ .namespace = "System", .name = "Any" },
            .null => .{ .namespace = "System", .name = "Any" },
        };
    }

    return .{ .namespace = "System", .name = "Any" };
}

fn itemIsBool(ctx: anytype, it: item.Item) bool {
    if (it.data_kind == .value and it.value != null and it.value.? == .boolean) return true;
    if (it.data_kind == .node_ref and it.node != null) {
        const A = @TypeOf(ctx.adapter.*);
        const ref = it.node.?;
        return A.kind(ctx.adapter, ref) == .bool;
    }
    return false;
}

fn itemBoolValue(ctx: anytype, it: item.Item) bool {
    if (it.data_kind == .value and it.value != null and it.value.? == .boolean) return it.value.?.boolean;
    if (it.data_kind == .node_ref and it.node != null) {
        const A = @TypeOf(ctx.adapter.*);
        const ref = it.node.?;
        return A.boolean(ctx.adapter, ref);
    }
    return false;
}

fn itemToValue(ctx: anytype, it: item.Item) item.Value {
    if (it.data_kind == .value and it.value != null) return it.value.?;
    if (it.data_kind == .node_ref and it.node != null) {
        const value_resolver = @import("value_resolver.zig");
        const A = @TypeOf(ctx.adapter.*);
        const ref = it.node.?;
        return value_resolver.nodeToValue(A, ctx.adapter, ref, it.type_id, ctx.schema);
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
            const integer_type_id = item.SystemTypeIds.integer;
            if (it.type_id != integer_type_id) return null;
            return std.fmt.parseInt(i64, s, 10) catch null;
        },
        else => return null,
    }
}

/// Converts an item to Long if possible.
/// - Integer or Long: passthrough/convert
/// - String: must match regex (\+|-)?\d+ and fit in 64 bits
/// - Boolean: true -> 1, false -> 0
/// - Other types: null (not convertible)
fn convertToLong(ctx: anytype, it: item.Item) ?i64 {
    const val = itemToValue(ctx, it);
    switch (val) {
        .integer => |v| return v,
        .long => |v| return v,
        .boolean => |v| return if (v) 1 else 0,
        .string => |s| return parseIntegerString(s),
        // Decimal is not convertible to Long
        else => return null,
    }
}

fn convertToBoolean(ctx: anytype, it: item.Item) ?bool {
    const val = itemToValue(ctx, it);
    const integer_type_id = item.SystemTypeIds.integer;
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
        // Integer->Decimal preserves integer precision (no decimal point)
        // This is important for lowBoundary/highBoundary to work correctly
        .integer => |v| std.fmt.allocPrint(ctx.allocator, "{d}", .{v}) catch null,
        .decimal => |s| s,
        // Spec: Boolean true->1.0, false->0.0 (single decimal place)
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

fn convertToString(ctx: anytype, it: item.Item) ?[]const u8 {
    const val = itemToValue(ctx, it);
    return switch (val) {
        .string => |s| s,
        .boolean => |v| if (v) "true" else "false",
        .integer => |v| std.fmt.allocPrint(ctx.allocator, "{d}", .{v}) catch null,
        .decimal => |s| s, // preserves original precision
        .date => |s| s, // already without @ prefix
        .time => |s| s, // already without @T prefix
        .dateTime => |s| s, // already without @ prefix
        .quantity => |q| blk: {
            // Calendar duration units are unquoted, UCUM units are quoted
            if (isCalendarDurationKeyword(q.unit)) {
                break :blk std.fmt.allocPrint(ctx.allocator, "{s} {s}", .{ q.value, q.unit }) catch null;
            } else {
                break :blk std.fmt.allocPrint(ctx.allocator, "{s} '{s}'", .{ q.value, q.unit }) catch null;
            }
        },
        else => null,
    };
}

fn canConvertToString(ctx: anytype, it: item.Item) bool {
    const val = itemToValue(ctx, it);
    return switch (val) {
        .boolean, .integer, .decimal, .string, .date, .time, .dateTime, .quantity => true,
        else => false,
    };
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

// Quantity value tuple for conversion functions
const QuantityParts = struct {
    value: []const u8,
    unit: []const u8,
};

// Parse a quantity string according to the FHIRPath spec regex:
// (?'value'(\+|-)?\d+(\.\d+)?)\s*('(?'unit'[^']+)'|(?'time'[a-zA-Z]+))?
// Returns (value, unit) where unit defaults to '1' if not provided.
fn parseQuantityString(s: []const u8) ?QuantityParts {
    if (s.len == 0) return null;

    var idx: usize = 0;

    // Parse optional sign
    if (idx < s.len and (s[idx] == '+' or s[idx] == '-')) {
        idx += 1;
    }

    // Parse integer part (required: at least one digit)
    const int_start = idx;
    while (idx < s.len and s[idx] >= '0' and s[idx] <= '9') {
        idx += 1;
    }
    if (idx == int_start) return null; // no digits

    // Parse optional decimal part
    if (idx < s.len and s[idx] == '.') {
        idx += 1;
        const dec_start = idx;
        while (idx < s.len and s[idx] >= '0' and s[idx] <= '9') {
            idx += 1;
        }
        if (idx == dec_start) return null; // decimal point but no digits
    }

    const value_end = idx;
    const value = s[0..value_end];

    // Skip whitespace
    while (idx < s.len and (s[idx] == ' ' or s[idx] == '\t')) {
        idx += 1;
    }

    // If nothing left, unit defaults to '1'
    if (idx >= s.len) {
        return .{ .value = value, .unit = "1" };
    }

    // Check for quoted unit: '...'
    if (s[idx] == '\'') {
        idx += 1;
        const unit_start = idx;
        // Find closing quote
        while (idx < s.len and s[idx] != '\'') {
            idx += 1;
        }
        if (idx >= s.len) return null; // no closing quote
        const unit = s[unit_start..idx];
        idx += 1; // skip closing quote

        // Should be at end of string (allow trailing whitespace)
        while (idx < s.len and (s[idx] == ' ' or s[idx] == '\t')) {
            idx += 1;
        }
        if (idx != s.len) return null; // extra characters after unit

        return .{ .value = value, .unit = unit };
    }

    // Check for calendar duration keyword: alphabetic characters only
    const time_start = idx;
    while (idx < s.len and ((s[idx] >= 'a' and s[idx] <= 'z') or (s[idx] >= 'A' and s[idx] <= 'Z'))) {
        idx += 1;
    }
    if (idx > time_start) {
        const time_unit = s[time_start..idx];

        // Should be at end of string (allow trailing whitespace)
        while (idx < s.len and (s[idx] == ' ' or s[idx] == '\t')) {
            idx += 1;
        }
        if (idx != s.len) return null; // extra characters after unit

        // Only accept valid calendar duration keywords as bare words
        // Per spec: year/years, month/months, week/weeks, day/days,
        // hour/hours, minute/minutes, second/seconds, millisecond/milliseconds
        if (!isCalendarDurationKeyword(time_unit)) return null;

        return .{ .value = value, .unit = time_unit };
    }

    // Something else - invalid
    return null;
}

// Check if a string is a valid FHIRPath calendar duration keyword
fn isCalendarDurationKeyword(s: []const u8) bool {
    const keywords = [_][]const u8{
        "year",   "years",
        "month",  "months",
        "week",   "weeks",
        "day",    "days",
        "hour",   "hours",
        "minute", "minutes",
        "second", "seconds",
        "millisecond", "milliseconds",
    };
    for (keywords) |kw| {
        if (std.mem.eql(u8, s, kw)) return true;
    }
    return false;
}

// Convert item to Quantity (for toQuantity)
// Returns (value, unit) tuple or null if conversion fails.
fn convertToQuantity(ctx: anytype, it: item.Item) ?QuantityParts {
    const val = itemToValue(ctx, it);
    switch (val) {
        .quantity => |q| return .{ .value = q.value, .unit = q.unit }, // passthrough
        .integer => |v| {
            // Integer becomes decimal string with unit '1'
            const value_str = std.fmt.allocPrint(ctx.allocator, "{d}", .{v}) catch return null;
            return .{ .value = value_str, .unit = "1" };
        },
        .decimal => |d| return .{ .value = d, .unit = "1" }, // decimal with unit '1'
        .boolean => |b| {
            // true -> 1.0 '1', false -> 0.0 '1'
            const value_str = if (b) "1.0" else "0.0";
            return .{ .value = value_str, .unit = "1" };
        },
        .string => |s| return parseQuantityString(s),
        else => return null,
    }
}

// Check if item can be converted to Quantity
fn canConvertToQuantity(ctx: anytype, it: item.Item) bool {
    const val = itemToValue(ctx, it);
    return switch (val) {
        .quantity, .integer, .decimal, .boolean => true,
        .string => |s| parseQuantityString(s) != null,
        else => false,
    };
}

fn typeIdForNode(ctx: anytype, node_ref: @TypeOf(ctx.adapter.*).NodeRef) u32 {
    const A = @TypeOf(ctx.adapter.*);
    return switch (A.kind(ctx.adapter, node_ref)) {
        .null => item.SystemTypeIds.any,
        .bool => item.SystemTypeIds.boolean,
        .number => if (isInteger(A.numberText(ctx.adapter, node_ref))) item.SystemTypeIds.integer else item.SystemTypeIds.decimal,
        .string => {
            const s = A.string(ctx.adapter, node_ref);
            if (isDateTime(s)) return item.SystemTypeIds.dateTime;
            if (isDate(s)) return item.SystemTypeIds.date;
            if (isTime(s)) return item.SystemTypeIds.time;
            return item.SystemTypeIds.string;
        },
        .object => {
            if (resourceTypeName(ctx, node_ref)) |rt| {
                if (ctx.schema) |s| {
                    if (s.typeIdByLocalName(rt)) |tid| return tid;
                }
            }
            return item.SystemTypeIds.any;
        },
        .array => item.SystemTypeIds.any,
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

/// Detect ISO-8601 dateTime: must start with YYYY-MM-DD and contain 'T'
/// Examples: "2020-01-15T12:00:00", "2020-01-15T12:00:00Z", "2020-01-15T12:00:00+05:00"
fn isDateTime(s: []const u8) bool {
    // Minimum length: YYYY-MM-DDTHH:MM = 16 chars, but allow YYYY-MM-DDT... = 11 minimum
    if (s.len < 11) return false;
    // Must contain 'T' after the date part
    const t_pos = std.mem.indexOfScalar(u8, s, 'T') orelse return false;
    // 'T' must come after a valid date portion (at least YYYY-MM-DD = 10 chars)
    if (t_pos < 10) return false;
    // Validate date portion before T
    return isValidDatePrefix(s[0..t_pos]);
}

/// Detect ISO-8601 date: YYYY, YYYY-MM, or YYYY-MM-DD without time component
fn isDate(s: []const u8) bool {
    // Minimum length: YYYY = 4 chars
    if (s.len < 4) return false;
    // Must not contain 'T' (that would be dateTime)
    if (std.mem.indexOfScalar(u8, s, 'T') != null) return false;
    return isValidDatePrefix(s);
}

/// Validate an ISO-8601 date prefix (before the 'T' for dateTime, or entire string for date)
/// Accepts: YYYY, YYYY-MM, YYYY-MM-DD
fn isValidDatePrefix(s: []const u8) bool {
    if (s.len < 4) return false;
    // First 4 chars must be digits (year)
    for (s[0..4]) |c| {
        if (c < '0' or c > '9') return false;
    }
    if (s.len == 4) return true; // YYYY
    // If longer, must have hyphen after year
    if (s.len < 5 or s[4] != '-') return false;
    // After YYYY-, we need at least MM (2 more chars)
    if (s.len < 7) return false;
    // Check month digits
    for (s[5..7]) |c| {
        if (c < '0' or c > '9') return false;
    }
    if (s.len == 7) return true; // YYYY-MM
    // If longer, must have hyphen after month
    if (s.len < 8 or s[7] != '-') return false;
    // After YYYY-MM-, we need at least DD (2 more chars)
    if (s.len < 10) return false;
    // Check day digits
    for (s[8..10]) |c| {
        if (c < '0' or c > '9') return false;
    }
    // YYYY-MM-DD (may have trailing timezone for dateTime, handled by caller)
    return true;
}

/// Detect ISO-8601 time: HH:MM or HH:MM:SS (without date, not starting with 'T')
fn isTime(s: []const u8) bool {
    // Time format: HH:MM or HH:MM:SS
    // Minimum length: HH:MM = 5 chars
    if (s.len < 5) return false;
    // Must contain ':' and NOT contain 'T' (would be dateTime)
    if (std.mem.indexOfScalar(u8, s, ':') == null) return false;
    if (std.mem.indexOfScalar(u8, s, 'T') != null) return false;
    // First 2 chars must be digits (hour)
    for (s[0..2]) |c| {
        if (c < '0' or c > '9') return false;
    }
    // Third char must be ':'
    if (s[2] != ':') return false;
    // Chars 3-4 must be digits (minute)
    for (s[3..5]) |c| {
        if (c < '0' or c > '9') return false;
    }
    return true;
}

// ============================================================================
// Math function implementations
// ============================================================================

const SumKind = enum { integer, long, decimal, quantity };

fn sumKindForValue(val: item.Value) ?SumKind {
    return switch (val) {
        .integer => .integer,
        .long => .long,
        .decimal => .decimal,
        .quantity => .quantity,
        else => null,
    };
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

fn makeDecimalItemText(_: anytype, text: []const u8) item.Item {
    const type_id = item.SystemTypeIds.decimal;
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
    const type_id = item.SystemTypeIds.decimal;
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

fn epochDaysToDate(days: i64) struct { year: i32, month: u32, day: u32 } {
    const z = days + 719468;
    const era = if (z >= 0) @divFloor(z, 146097) else @divFloor(z - 146096, 146097);
    const doe: u32 = @intCast(z - era * 146097);
    const yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    const y = @as(i32, @intCast(yoe)) + @as(i32, @intCast(era)) * 400;
    const doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    const mp = (5 * doy + 2) / 153;
    const d = doy - (153 * mp + 2) / 5 + 1;
    const m = if (mp < 10) mp + 3 else mp - 9;
    const year = y + (if (m <= 2) @as(i32, 1) else 0);
    return .{ .year = year, .month = m, .day = d };
}

fn formatDateTime(allocator: std.mem.Allocator, timestamp: i64) ![]const u8 {
    const s = timestamp;
    const epoch_days = @divFloor(s, std.time.s_per_day);
    const day_seconds_signed = @mod(s, std.time.s_per_day);
    const day_seconds: u64 = @intCast(if (day_seconds_signed < 0) day_seconds_signed + std.time.s_per_day else day_seconds_signed);
    
    const date = epochDaysToDate(epoch_days);
    
    const hour = day_seconds / std.time.s_per_hour;
    const minute = (day_seconds % std.time.s_per_hour) / std.time.s_per_min;
    const second = day_seconds % std.time.s_per_min;
    
    const y: u32 = @intCast(date.year);
    return std.fmt.allocPrint(allocator, "{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}+00:00", .{y, date.month, date.day, hour, minute, second});
}

fn formatDate(allocator: std.mem.Allocator, timestamp: i64) ![]const u8 {
    const s = timestamp;
    const epoch_days = @divFloor(s, std.time.s_per_day);
    const date = epochDaysToDate(epoch_days);
    
    const y: u32 = @intCast(date.year);
    return std.fmt.allocPrint(allocator, "{d:0>4}-{d:0>2}-{d:0>2}", .{y, date.month, date.day});
}

fn formatTime(allocator: std.mem.Allocator, timestamp: i64) ![]const u8 {
    const s = timestamp;
    const day_seconds_signed = @mod(s, std.time.s_per_day);
    const day_seconds: u64 = @intCast(if (day_seconds_signed < 0) day_seconds_signed + std.time.s_per_day else day_seconds_signed);
    
    const hour = day_seconds / std.time.s_per_hour;
    const minute = (day_seconds % std.time.s_per_hour) / std.time.s_per_min;
    const second = day_seconds % std.time.s_per_min;
    
    return std.fmt.allocPrint(allocator, "{d:0>2}:{d:0>2}:{d:0>2}", .{hour, minute, second});
}

fn evalDuration(ctx: anytype, input: item.Item, other: item.Item, precision: []const u8) EvalError!ItemList {
    return calculateDurationDifference(ctx, input, other, precision, false);
}

fn evalDifference(ctx: anytype, input: item.Item, other: item.Item, precision: []const u8) EvalError!ItemList {
    return calculateDurationDifference(ctx, input, other, precision, true);
}

fn calculateDurationDifference(ctx: anytype, input: item.Item, other: item.Item, precision: []const u8, is_difference: bool) EvalError!ItemList {
    var out = ItemList.empty;
    const val_input = itemToValue(ctx, input);
    const val_other = itemToValue(ctx, other);

    // Normalize precision
    const unit = parseTimeUnit(precision) orelse return out;

    // Extract values based on type
    var date_a: ?DateParts = null;
    var date_b: ?DateParts = null;
    var time_a: ?TimePartsPartial = null;
    var time_b: ?TimePartsPartial = null;
    var is_date = false;
    var is_time = false;

    // Input A
    if (val_input == .date) {
        date_a = parseDateParts(stripAtPrefix(val_input.date));
        is_date = true;
    } else if (val_input == .dateTime) {
        const s = val_input.dateTime;
        const dt = parseDateTimeParts(stripAtPrefix(s));
        if (dt) |d| {
            date_a = d.date;
            time_a = parseTimePartsPartial(stripTimePrefix(s[s.len - d.time.hour.len - (if (d.time.zone) |z| z.len else 0) - (if (d.time.second) |sec| sec.len + 1 else 0) - (if (d.time.minute.len > 0) d.time.minute.len + 1 else 0) ..]));
            // Re-parse time properly
            if (std.mem.indexOfScalar(u8, s, 'T')) |t_idx| {
                time_a = parseTimePartsPartial(s[t_idx + 1 ..]);
            }
        }
    } else if (val_input == .time) {
        time_a = parseTimePartsPartial(stripTimePrefix(stripAtPrefix(val_input.time)));
        is_time = true;
    }

    // Input B
    if (val_other == .date) {
        date_b = parseDateParts(stripAtPrefix(val_other.date));
        if (!is_date) return out; // Mismatched types (Date vs non-Date)
    } else if (val_other == .dateTime) {
        const s = val_other.dateTime;
        const dt = parseDateTimeParts(stripAtPrefix(s));
        if (dt) |d| {
            date_b = d.date;
            if (std.mem.indexOfScalar(u8, s, 'T')) |t_idx| {
                time_b = parseTimePartsPartial(s[t_idx + 1 ..]);
            }
        }
        if (is_date or is_time) return out; // Mismatched
    } else if (val_other == .time) {
        time_b = parseTimePartsPartial(stripTimePrefix(stripAtPrefix(val_other.time)));
        if (!is_time) return out; // Mismatched
    }

    if (is_date) {
        // Date comparison
        if (date_a == null or date_b == null) return out;
        // Precision check: Date can calculate year, month, week, day
        // For day/week, we need full YMD. For month, YM. For year, Y.
        // Actually, spec says: if input precision < requested precision, return empty.
        // Date strings are YYYY, YYYY-MM, YYYY-MM-DD.
        // year requires Y
        // month requires Y-M
        // day/week requires Y-M-D
        
        const prec_a: i32 = if (date_a.?.day != null) 3 else if (date_a.?.month != null) 2 else 1;
        const prec_b: i32 = if (date_b.?.day != null) 3 else if (date_b.?.month != null) 2 else 1;
        const req_prec: i32 = switch (unit) {
            .year => 1,
            .month => 2,
            .week, .day => 3,
            else => return out, // Date doesn't support time units
        };
        if (prec_a < req_prec or prec_b < req_prec) return out;

        const y_a = std.fmt.parseInt(i32, date_a.?.year, 10) catch return out;
        const m_a = if (date_a.?.month) |m| std.fmt.parseInt(i32, m, 10) catch return out else 1;
        const d_a = if (date_a.?.day) |d| std.fmt.parseInt(i32, d, 10) catch return out else 1;

        const y_b = std.fmt.parseInt(i32, date_b.?.year, 10) catch return out;
        const m_b = if (date_b.?.month) |m| std.fmt.parseInt(i32, m, 10) catch return out else 1;
        const d_b = if (date_b.?.day) |d| std.fmt.parseInt(i32, d, 10) catch return out else 1;

        // Determine sign
        // duration/difference(a, b) -> if a > b, result is negative
        // So we calculate b - a
        const cmp = compareDate(y_a, m_a, d_a, y_b, m_b, d_b);
        if (cmp == 0) {
            try out.append(ctx.allocator, makeIntegerItem(ctx, 0));
            return out;
        }
        
        // Ensure A is the earlier date for calculation, flip sign at end if needed
        const a_first = cmp < 0;
        const start_y = if (a_first) y_a else y_b;
        const start_m = if (a_first) m_a else m_b;
        const start_d = if (a_first) d_a else d_b;
        const end_y = if (a_first) y_b else y_a;
        const end_m = if (a_first) m_b else m_a;
        const end_d = if (a_first) d_b else d_a;

        var result: i64 = 0;

        switch (unit) {
            .year => {
                if (is_difference) {
                    result = end_y - start_y;
                } else {
                    // Whole years
                    var diff = end_y - start_y;
                    // If end month/day is before start month/day, subtract one year
                    if (end_m < start_m or (end_m == start_m and end_d < start_d)) {
                        diff -= 1;
                    }
                    result = diff;
                }
            },
            .month => {
                const months_diff = (end_y - start_y) * 12 + (end_m - start_m);
                if (is_difference) {
                    result = months_diff;
                } else {
                    // Whole months
                    var diff = months_diff;
                    // If end day is before start day, subtract one month
                    // Exception: if start day is 31 and end day is 28/29 (Feb), it might be a full month?
                    // Spec example: Jan 31 to Feb 28 is NOT a full month.
                    // Jan 15 to Feb 15 IS a full month.
                    if (end_d < start_d) {
                        diff -= 1;
                    }
                    result = diff;
                }
            },
            .week => {
                if (is_difference) {
                    // Count Sunday boundaries crossed
                    // Find first Sunday on or after start
                    // Day of week: 0=Sunday, 1=Monday...
                    // epochDaysToDate gives us absolute days.
                    // We need dateToEpochDays.
                    const start_epoch = dateToEpochDays(start_y, @intCast(start_m), @intCast(start_d));
                    const end_epoch = dateToEpochDays(end_y, @intCast(end_m), @intCast(end_d));
                    
                    // Known Sunday: 2000-01-05 (start_epoch + offset)
                    // Let's implement dayOfWeek
                    // 1970-01-01 was Thursday (4).
                    // (days + 4) % 7. 0=Sunday.
                    
                    var current = start_epoch;
                    var boundaries: i64 = 0;
                    while (current < end_epoch) {
                        current += 1;
                        if (@mod(current + 4, 7) == 0) boundaries += 1;
                    }
                    result = boundaries;
                } else {
                    // Whole weeks = days / 7
                    const start_epoch = dateToEpochDays(start_y, @intCast(start_m), @intCast(start_d));
                    const end_epoch = dateToEpochDays(end_y, @intCast(end_m), @intCast(end_d));
                    result = @divTrunc(end_epoch - start_epoch, 7);
                }
            },
            .day => {
                const start_epoch = dateToEpochDays(start_y, @intCast(start_m), @intCast(start_d));
                const end_epoch = dateToEpochDays(end_y, @intCast(end_m), @intCast(end_d));
                result = end_epoch - start_epoch;
            },
            else => unreachable,
        }

        if (!a_first) result = -result;
        try out.append(ctx.allocator, makeIntegerItem(ctx, result));
        return out;
    } else if (is_time) {
        if (time_a == null or time_b == null) return out;
        
        // Time arithmetic
        const ms_a = timeToMillis(time_a.?);
        const ms_b = timeToMillis(time_b.?);
        
        // Calculate difference in requested unit
        var result: i64 = 0;
        const a_first = ms_a <= ms_b;
        const start_ms = if (a_first) ms_a else ms_b;
        const end_ms = if (a_first) ms_b else ms_a;
        
        switch (unit) {
            .hour => {
                if (is_difference) {
                    result = @divTrunc(end_ms, 3600000) - @divTrunc(start_ms, 3600000);
                } else {
                    result = @divTrunc(end_ms - start_ms, 3600000);
                }
            },
            .minute => {
                if (is_difference) {
                    result = @divTrunc(end_ms, 60000) - @divTrunc(start_ms, 60000);
                } else {
                    result = @divTrunc(end_ms - start_ms, 60000);
                }
            },
            .second => {
                if (is_difference) {
                    result = @divTrunc(end_ms, 1000) - @divTrunc(start_ms, 1000);
                } else {
                    result = @divTrunc(end_ms - start_ms, 1000);
                }
            },
            .millisecond => {
                result = end_ms - start_ms;
            },
            else => return out, // Time doesn't support year/month/week/day
        }
        
        if (!a_first) result = -result;
        try out.append(ctx.allocator, makeIntegerItem(ctx, result));
        return out;

    } else {
        // DateTime comparison
        if (date_a == null or date_b == null) return out;
        
        // Check precision compatibility
        var prec_a: i32 = 1;
        if (time_a != null) {
            prec_a = 3 + (if (time_a.?.second != null) @as(i32, 2) else @as(i32, 1));
        } else {
            if (date_a.?.day != null) prec_a = 3
            else if (date_a.?.month != null) prec_a = 2
            else prec_a = 1;
        }

        var prec_b: i32 = 1;
        if (time_b != null) {
            prec_b = 3 + (if (time_b.?.second != null) @as(i32, 2) else @as(i32, 1));
        } else {
            if (date_b.?.day != null) prec_b = 3
            else if (date_b.?.month != null) prec_b = 2
            else prec_b = 1;
        }
        
        // Approximate precision check (not perfect but covers basic cases)
        const req_prec: i32 = switch (unit) {
            .year => 1,
            .month => 2,
            .week, .day => 3,
            .hour => 4,
            .minute => 5,
            .second, .millisecond => 6,
        };
        if (prec_a < req_prec or prec_b < req_prec) return out;

        // Parse date components
        const y_a = std.fmt.parseInt(i32, date_a.?.year, 10) catch return out;
        const m_a = if (date_a.?.month) |m| std.fmt.parseInt(i32, m, 10) catch return out else 1;
        const d_a = if (date_a.?.day) |d| std.fmt.parseInt(i32, d, 10) catch return out else 1;

        const y_b = std.fmt.parseInt(i32, date_b.?.year, 10) catch return out;
        const m_b = if (date_b.?.month) |m| std.fmt.parseInt(i32, m, 10) catch return out else 1;
        const d_b = if (date_b.?.day) |d| std.fmt.parseInt(i32, d, 10) catch return out else 1;

        // For year/month, we don't need time, just calendar math
        if (unit == .year or unit == .month) {
            // Determine sign based on full datetime comparison
            const epoch_a = dateTimeToEpochMillis(y_a, @intCast(m_a), @intCast(d_a), time_a);
            const epoch_b = dateTimeToEpochMillis(y_b, @intCast(m_b), @intCast(d_b), time_b);
            
            const cmp: i32 = if (epoch_a < epoch_b) -1 else if (epoch_a > epoch_b) 1 else 0;
            if (cmp == 0) {
                try out.append(ctx.allocator, makeIntegerItem(ctx, 0));
                return out;
            }
            
            const a_first = cmp < 0;
            const start_y = if (a_first) y_a else y_b;
            const start_m = if (a_first) m_a else m_b;
            const start_d = if (a_first) d_a else d_b;
            const end_y = if (a_first) y_b else y_a;
            const end_m = if (a_first) m_b else m_a;
            const end_d = if (a_first) d_b else d_a;
            
            var result: i64 = 0;
            if (unit == .year) {
                if (is_difference) {
                    result = end_y - start_y;
                } else {
                    var diff = end_y - start_y;
                    // Check if end is before start in the year (ignoring year part)
                    // Construct dummy dates in same year to compare remaining components
                    // Or simpler: compare epoch of start + diff years with end
                    // But adding years is tricky with leap years.
                    // Simple check: if end < start + diff years, decrement.
                    // Logic: if (end_month < start_month) or (end_month == start_month and end_day < start_day) ...
                    // We need to account for time too.
                    // compare (m_a, d_a, time_a) vs (m_b, d_b, time_b)
                    const sub_a = dateTimeToEpochMillis(2000, @intCast(start_m), @intCast(start_d), if (a_first) time_a else time_b);
                    const sub_b = dateTimeToEpochMillis(2000, @intCast(end_m), @intCast(end_d), if (a_first) time_b else time_a);
                    if (sub_b < sub_a) diff -= 1;
                    result = diff;
                }
            } else { // month
                const months_diff = (end_y - start_y) * 12 + (end_m - start_m);
                if (is_difference) {
                    result = months_diff;
                }
                else {
                    var diff = months_diff;
                    // Compare day+time
                    // Check if end day+time is before start day+time
                    const sub_a = dateTimeToEpochMillis(2000, 1, @intCast(start_d), if (a_first) time_a else time_b);
                    const sub_b = dateTimeToEpochMillis(2000, 1, @intCast(end_d), if (a_first) time_b else time_a);
                    if (sub_b < sub_a) diff -= 1;
                    result = diff;
                }
            }
            
            if (!a_first) result = -result;
            try out.append(ctx.allocator, makeIntegerItem(ctx, result));
            return out;
        }

        // For other units, convert to epoch milliseconds
        const ms_a = dateTimeToEpochMillis(y_a, @intCast(m_a), @intCast(d_a), time_a);
        const ms_b = dateTimeToEpochMillis(y_b, @intCast(m_b), @intCast(d_b), time_b);
        
        var result: i64 = 0;
        const a_first = ms_a <= ms_b;
        const start_ms = if (a_first) ms_a else ms_b;
        const end_ms = if (a_first) ms_b else ms_a;
        
        switch (unit) {
            .week => {
                if (is_difference) {
                    // Count Sunday boundaries
                    // Epoch 1970-01-01 was Thursday.
                    // ms / 86400000 -> days. (days + 4) % 7 == 0 is Sunday.
                    // We need to account for timezone offset if we want "local" boundaries?
                    // Spec says: "When comparison based on precision... implementations should normalize timezones".
                    // This implies UTC.
                    // Let's use UTC days.
                    const start_day = @divFloor(start_ms, 86400000);
                    const end_day = @divFloor(end_ms, 86400000);
                    var current = start_day;
                    var boundaries: i64 = 0;
                    while (current < end_day) {
                        current += 1;
                        if (@mod(current + 4, 7) == 0) boundaries += 1;
                    }
                    result = boundaries;
                } else {
                    // duration in weeks = duration in days / 7
                    const diff_ms = end_ms - start_ms;
                    result = @divTrunc(diff_ms, 604800000);
                }
            },
            .day => {
                if (is_difference) {
                    result = @divFloor(end_ms, 86400000) - @divFloor(start_ms, 86400000);
                } else {
                    result = @divTrunc(end_ms - start_ms, 86400000);
                }
            },
            .hour => {
                if (is_difference) {
                    result = @divFloor(end_ms, 3600000) - @divFloor(start_ms, 3600000);
                } else {
                    result = @divTrunc(end_ms - start_ms, 3600000);
                }
            },
            .minute => {
                if (is_difference) {
                    result = @divFloor(end_ms, 60000) - @divFloor(start_ms, 60000);
                } else {
                    result = @divTrunc(end_ms - start_ms, 60000);
                }
            },
            .second => {
                if (is_difference) {
                    result = @divFloor(end_ms, 1000) - @divFloor(start_ms, 1000);
                } else {
                    result = @divTrunc(end_ms - start_ms, 1000);
                }
            },
            .millisecond => {
                result = end_ms - start_ms;
            },
            else => unreachable,
        }
        
        if (!a_first) result = -result;
        try out.append(ctx.allocator, makeIntegerItem(ctx, result));
        return out;
    }
}

fn timeToMillis(t: TimePartsPartial) i64 {
    const h = std.fmt.parseInt(i64, t.hour, 10) catch 0;
    const m = if (t.minute) |min| std.fmt.parseInt(i64, min, 10) catch 0 else 0;
    const s_part = if (t.second) |sec| splitSecond(sec) else SecondParts{ .whole = "0", .frac = null };
    const s = std.fmt.parseInt(i64, s_part.whole, 10) catch 0;
    const ms = if (s_part.frac) |f| parseMillis(f) else 0;
    return h * 3600000 + m * 60000 + s * 1000 + ms;
}

fn parseMillis(f: []const u8) i64 {
    var buf: [3]u8 = .{'0', '0', '0'};
    const len = @min(f.len, 3);
    @memcpy(buf[0..len], f[0..len]);
    return std.fmt.parseInt(i64, &buf, 10) catch 0;
}

fn compareDate(y1: i32, m1: i32, d1: i32, y2: i32, m2: i32, d2: i32) i32 {
    if (y1 != y2) return if (y1 < y2) -1 else 1;
    if (m1 != m2) return if (m1 < m2) -1 else 1;
    if (d1 != d2) return if (d1 < d2) -1 else 1;
    return 0;
}

fn dateToEpochDays(year: i32, month: u32, day: u32) i64 {
    var y: i64 = year;
    const m: i64 = month;
    const d: i64 = day;
    if (m <= 2) y -= 1;
    const era = if (y >= 0) @divFloor(y, 400) else @divFloor(y - 399, 400);
    const yoe: u64 = @intCast(y - era * 400);
    const doy: u64 = @as(u64, @intCast(153 * (if (m > 2) m - 3 else m + 9) + 2)) / 5 + @as(u64, @intCast(d)) - 1;
    const doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    return era * 146097 + @as(i64, @intCast(doe)) - 719468;
}

fn dateTimeToEpochMillis(year: i32, month: u32, day: u32, time: ?TimePartsPartial) i64 {
    const days = dateToEpochDays(year, month, day);
    var ms = days * 86400000;
    if (time) |t| {
        ms += timeToMillis(t);
        // Handle timezone
        if (t.zone) |z| {
            if (z.len >= 1 and z[0] != 'Z') {
                const sign: i64 = if (z[0] == '+') 1 else -1;
                // Parse +HH:MM
                if (z.len >= 6) {
                    const zh = std.fmt.parseInt(i64, z[1..3], 10) catch 0;
                    const zm = std.fmt.parseInt(i64, z[4..6], 10) catch 0;
                    const offset_ms = (zh * 3600000 + zm * 60000) * sign;
                    ms -= offset_ms; // Subtract offset to get UTC
                }
            }
        }
    }
    return ms;
}
