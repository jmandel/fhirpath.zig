const std = @import("std");
const lib = @import("lib.zig");
const ast = @import("ast.zig");
const eval = @import("eval.zig");
const item = @import("item.zig");
const schema = @import("schema.zig");
const JsonAdapter = @import("backends/json_adapter.zig").JsonAdapter;
const XmlAdapter = @import("backends/xml_adapter.zig").XmlAdapter;
const xml_parser = @import("backends/xml_parser.zig");
const JsAdapter = @import("backends/js_adapter.zig").JsAdapter;

pub const Status = enum(u32) {
    ok = 0,
    parse_error = 1,
    runtime_error = 2,
    type_error = 3,
    singleton_required = 4,
    order_undefined = 5,
    output_overflow = 6,
    arena_oom = 7,
    model_error = 8,
    invalid_options = 9,
    unsupported = 10,
};

pub const DecimalOut = extern struct {
    sign: i32,
    scale: u32,
    norm_scale: u32,
    mag0: u32,
    mag1: u32,
    mag2: u32,
    mag3: u32,
};

pub const QuantityOut = extern struct {
    dec: DecimalOut,
    unit_ptr: u32,
    unit_len: u32,
};

const SchemaEntry = struct {
    bytes: []u8,
    schema: schema.Schema,
};

const Context = struct {
    allocator: std.mem.Allocator,
    schemas: std.StringHashMap(SchemaEntry),
    default_schema: ?[]const u8,
    last_schema: ?*schema.Schema,
    now_epoch_seconds: i64,
    result: ?eval.EvalResult,
    iters: std.ArrayListUnmanaged(usize) = .{},

    fn init(allocator: std.mem.Allocator) !*Context {
        const ctx = try allocator.create(Context);
        ctx.* = .{
            .allocator = allocator,
            .schemas = std.StringHashMap(SchemaEntry).init(allocator),
            .default_schema = null,
            .last_schema = null,
            .now_epoch_seconds = 0,
            .result = null,
            .iters = .{},
        };
        return ctx;
    }

    fn resetResult(self: *Context) void {
        if (self.result) |*res| {
            res.deinit();
        }
        self.result = null;
        self.iters.clearRetainingCapacity();
    }

    fn deinit(self: *Context) void {
        self.resetResult();
        var it = self.schemas.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.schema.deinit();
            self.allocator.free(entry.value_ptr.bytes);
        }
        self.schemas.deinit();
        self.iters.deinit(self.allocator);
        self.allocator.destroy(self);
    }
};

fn alloc() std.mem.Allocator {
    return std.heap.wasm_allocator;
}

var contexts: std.ArrayListUnmanaged(?*Context) = .{};

fn memSlice(ptr: u32, len: u32) []const u8 {
    if (len == 0) return &[_]u8{};
    return @as([*]const u8, @ptrFromInt(ptr))[0..len];
}

fn ctxFromHandle(handle: u32) ?*Context {
    if (handle == 0) return null;
    const idx: usize = @intCast(handle - 1);
    if (idx >= contexts.items.len) return null;
    return contexts.items[idx];
}

fn itemFromHandle(ctx: *Context, handle: u32) ?item.Item {
    if (handle == 0) return null;
    if (ctx.result == null) return null;
    const idx: usize = @intCast(handle - 1);
    if (idx >= ctx.result.?.items.items.len) return null;
    return ctx.result.?.items.items[idx];
}

/// Get the resolved std.json.Value for a node_ref item.
fn nodeJsonValue(ctx: *Context, it: item.Item) ?std.json.Value {
    const res = ctx.result orelse return null;
    return res.nodeValue(it);
}

/// Get the JSON text for a node_ref item (serialized on demand into result arena).
fn nodeTextSlice(ctx: *Context, it: item.Item) ?[]const u8 {
    const jv = nodeJsonValue(ctx, it) orelse return null;
    const res = &(ctx.result.?);
    const arena_alloc = res.arena.allocator();
    return std.json.Stringify.valueAlloc(arena_alloc, jv, .{}) catch return null;
}

/// Get the string or number text from a node_ref item.
fn nodeStringSlice(ctx: *Context, it: item.Item) ?[]const u8 {
    const jv = nodeJsonValue(ctx, it) orelse return null;
    return switch (jv) {
        .string => |s| s,
        .number_string => |s| s,
        else => null,
    };
}

fn typeNameSlice(ctx: *Context, type_id: u32) []const u8 {
    if (type_id == 0) return "";
    if (ctx.last_schema) |s| return s.outputTypeName(type_id);
    if (schema.isModelType(type_id)) return "";
    return schema.systemTypeName(type_id);
}

fn parseDecimal(text: []const u8, out: *DecimalOut) bool {
    if (text.len == 0) return false;
    var s = text;
    var sign: i32 = 1;
    if (s[0] == '-') {
        sign = -1;
        s = s[1..];
    } else if (s[0] == '+') {
        s = s[1..];
    }
    if (s.len == 0) return false;

    var mag: u128 = 0;
    var scale: u32 = 0;
    var saw_dot = false;
    var saw_digit = false;

    for (s) |c| {
        if (c == '.') {
            if (saw_dot) return false;
            saw_dot = true;
            continue;
        }
        if (c < '0' or c > '9') return false;
        saw_digit = true;
        const digit: u8 = c - '0';
        const next = mag * 10 + digit;
        if (next < mag) return false; // overflow
        mag = next;
        if (saw_dot) scale += 1;
    }

    if (!saw_digit) return false;

    // Trim trailing zeros from fractional part for norm_scale.
    var norm_scale = scale;
    if (scale > 0) {
        var i: isize = @intCast(s.len - 1);
        var remaining_frac: u32 = scale;
        while (i >= 0 and remaining_frac > 0) : (i -= 1) {
            const c = s[@intCast(i)];
            if (c == '.') continue;
            if (c != '0') break;
            norm_scale -= 1;
            remaining_frac -= 1;
            mag = mag / 10;
        }
    }

    out.* = .{
        .sign = sign,
        .scale = scale,
        .norm_scale = norm_scale,
        .mag0 = @intCast(mag & 0xFFFF_FFFF),
        .mag1 = @intCast((mag >> 32) & 0xFFFF_FFFF),
        .mag2 = @intCast((mag >> 64) & 0xFFFF_FFFF),
        .mag3 = @intCast((mag >> 96) & 0xFFFF_FFFF),
    };
    return true;
}

fn statusFromError(err: anyerror) Status {
    return switch (err) {
        error.OutOfMemory => .arena_oom,
        error.InvalidJson, error.TrailingData => .parse_error,
        error.InvalidFunction, error.InvalidPredicate, error.SingletonRequired, error.InvalidOperand => .runtime_error,
        error.UnexpectedToken, error.Unterminated, error.LexerError => .parse_error,
        else => .runtime_error,
    };
}

pub export fn fhirpath_alloc(len: u32) u32 {
    const allocator = alloc();
    const buf = allocator.alloc(u8, len) catch return 0;
    return @intCast(@intFromPtr(buf.ptr));
}

pub export fn fhirpath_free(ptr: u32, len: u32) void {
    if (ptr == 0 or len == 0) return;
    const allocator = alloc();
    const slice = @as([*]u8, @ptrFromInt(ptr))[0..len];
    allocator.free(slice);
}

pub export fn fhirpath_ctx_new() u32 {
    const allocator = alloc();
    const ctx = Context.init(allocator) catch return 0;
    contexts.append(alloc(), ctx) catch {
        ctx.deinit();
        return 0;
    };
    return @intCast(contexts.items.len);
}

pub export fn fhirpath_ctx_free(ctx_handle: u32) void {
    const idx: usize = if (ctx_handle == 0) return else @intCast(ctx_handle - 1);
    if (idx >= contexts.items.len) return;
    if (contexts.items[idx]) |ctx| {
        ctx.deinit();
        contexts.items[idx] = null;
    }
}

pub export fn fhirpath_ctx_register_schema(
    ctx_handle: u32,
    name_ptr: u32,
    name_len: u32,
    prefix_ptr: u32,
    prefix_len: u32,
    model_ptr: u32,
    model_len: u32,
    is_default: u32,
) Status {
    const ctx = ctxFromHandle(ctx_handle) orelse return .invalid_options;
    if (name_len == 0) return .invalid_options;

    const name = memSlice(name_ptr, name_len);
    const prefix = memSlice(prefix_ptr, prefix_len);
    const model_src = memSlice(model_ptr, model_len);

    const model_copy = ctx.allocator.dupe(u8, model_src) catch return .arena_oom;
    var schema_obj = schema.Schema.init(ctx.allocator, name, prefix, model_copy) catch {
        ctx.allocator.free(model_copy);
        return .model_error;
    };

    if (ctx.schemas.fetchRemove(schema_obj.name)) |old_entry| {
        var old = old_entry;
        old.value.schema.deinit();
        ctx.allocator.free(old.value.bytes);
    }

    ctx.schemas.put(schema_obj.name, .{ .bytes = model_copy, .schema = schema_obj }) catch {
        schema_obj.deinit();
        ctx.allocator.free(model_copy);
        return .arena_oom;
    };

    if (is_default != 0) {
        ctx.default_schema = schema_obj.name;
    }

    return .ok;
}

pub export fn fhirpath_ctx_set_time(ctx_handle: u32, epoch_seconds: i64) Status {
    const ctx = ctxFromHandle(ctx_handle) orelse return .invalid_options;
    ctx.now_epoch_seconds = epoch_seconds;
    return .ok;
}

pub export fn fhirpath_eval(
    ctx_handle: u32,
    expr_ptr: u32,
    expr_len: u32,
    json_ptr: u32,
    json_len: u32,
    schema_name_ptr: u32,
    schema_name_len: u32,
    fhir_mode: u32,
    opts_ptr: u32,
    opts_len: u32,
) Status {
    _ = opts_ptr;
    if (opts_len != 0) return .invalid_options;

    const ctx = ctxFromHandle(ctx_handle) orelse return .invalid_options;
    ctx.resetResult();

    const expr_text = memSlice(expr_ptr, expr_len);
    const json_text = memSlice(json_ptr, json_len);

    var schema_ptr: ?*schema.Schema = null;
    if (schema_name_len > 0) {
        const schema_name = memSlice(schema_name_ptr, schema_name_len);
        if (ctx.schemas.getPtr(schema_name)) |entry| {
            schema_ptr = &entry.schema;
        } else {
            return .model_error;
        }
    } else if (ctx.default_schema) |def| {
        if (ctx.schemas.getPtr(def)) |entry| {
            schema_ptr = &entry.schema;
        } else {
            return .model_error;
        }
    }

    const expr = lib.parseExpression(ctx.allocator, expr_text) catch |err| {
        return statusFromError(err);
    };
    defer ast.deinitExpr(ctx.allocator, expr);

    {
        const flavor: JsonAdapter.Flavor = if (fhir_mode != 0) .fhir_json else .generic_json;
        var arena = std.heap.ArenaAllocator.init(ctx.allocator);
        const arena_alloc = arena.allocator();
        const root_val = arena_alloc.create(std.json.Value) catch {
            arena.deinit();
            return .arena_oom;
        };
        const parse_opts: std.json.ParseOptions = if (schema_ptr != null) .{} else .{ .parse_numbers = false };
        root_val.* = std.json.parseFromSliceLeaky(std.json.Value, arena_alloc, json_text, parse_opts) catch |err| {
            arena.deinit();
            return statusFromError(err);
        };
        var adapter = JsonAdapter.init(arena_alloc, root_val, flavor);
        var eval_ctx = eval.EvalContext(JsonAdapter){
            .allocator = arena_alloc,
            .adapter = &adapter,

            .schema = schema_ptr,
            .timestamp = ctx.now_epoch_seconds,
        };
        const items = eval.evalExpression(&eval_ctx, expr, adapter.root(), null) catch |err| {
            arena.deinit();
            return statusFromError(err);
        };
        ctx.result = eval.resolveResult(JsonAdapter, &adapter, items, &arena, schema_ptr) catch |err| {
            arena.deinit();
            return statusFromError(err);
        };
    }
    ctx.last_schema = schema_ptr;
    return .ok;
}

pub export fn fhirpath_eval_xml(
    ctx_handle: u32,
    expr_ptr: u32,
    expr_len: u32,
    xml_ptr: u32,
    xml_len: u32,
    schema_name_ptr: u32,
    schema_name_len: u32,
    opts_ptr: u32,
    opts_len: u32,
) Status {
    _ = opts_ptr;
    if (opts_len != 0) return .invalid_options;

    const ctx = ctxFromHandle(ctx_handle) orelse return .invalid_options;
    ctx.resetResult();

    const expr_text = memSlice(expr_ptr, expr_len);
    const xml_text = memSlice(xml_ptr, xml_len);

    var schema_ptr: ?*schema.Schema = null;
    if (schema_name_len > 0) {
        const schema_name = memSlice(schema_name_ptr, schema_name_len);
        if (ctx.schemas.getPtr(schema_name)) |entry| {
            schema_ptr = &entry.schema;
        } else {
            return .model_error;
        }
    } else if (ctx.default_schema) |def| {
        if (ctx.schemas.getPtr(def)) |entry| {
            schema_ptr = &entry.schema;
        } else {
            return .model_error;
        }
    }

    const expr = lib.parseExpression(ctx.allocator, expr_text) catch |err| {
        return statusFromError(err);
    };
    defer ast.deinitExpr(ctx.allocator, expr);

    {
        var arena = std.heap.ArenaAllocator.init(ctx.allocator);
        const arena_alloc = arena.allocator();
        const root_node = xml_parser.parse(arena_alloc, xml_text) catch {
            arena.deinit();
            return .parse_error;
        };
        var adapter = XmlAdapter.init(arena_alloc, root_node);
        var eval_ctx = eval.EvalContext(XmlAdapter){
            .allocator = arena_alloc,
            .adapter = &adapter,

            .schema = schema_ptr,
            .timestamp = ctx.now_epoch_seconds,
        };
        const items = eval.evalExpression(&eval_ctx, expr, adapter.root(), null) catch |err| {
            arena.deinit();
            return statusFromError(err);
        };
        ctx.result = eval.resolveResult(XmlAdapter, &adapter, items, &arena, schema_ptr) catch |err| {
            arena.deinit();
            return statusFromError(err);
        };
    }
    ctx.last_schema = schema_ptr;
    return .ok;
}

pub export fn fhirpath_eval_js(
    ctx_handle: u32,
    expr_ptr: u32,
    expr_len: u32,
    root_js_id: u32,
    schema_name_ptr: u32,
    schema_name_len: u32,
    fhir_mode: u32,
    opts_ptr: u32,
    opts_len: u32,
) Status {
    _ = opts_ptr;
    if (opts_len != 0) return .invalid_options;

    const ctx = ctxFromHandle(ctx_handle) orelse return .invalid_options;
    ctx.resetResult();

    const expr_text = memSlice(expr_ptr, expr_len);

    var schema_ptr: ?*schema.Schema = null;
    if (schema_name_len > 0) {
        const schema_name = memSlice(schema_name_ptr, schema_name_len);
        if (ctx.schemas.getPtr(schema_name)) |entry| {
            schema_ptr = &entry.schema;
        } else {
            return .model_error;
        }
    } else if (ctx.default_schema) |def| {
        if (ctx.schemas.getPtr(def)) |entry| {
            schema_ptr = &entry.schema;
        } else {
            return .model_error;
        }
    }

    const expr = lib.parseExpression(ctx.allocator, expr_text) catch |err| {
        return statusFromError(err);
    };
    defer ast.deinitExpr(ctx.allocator, expr);

    {
        var arena = std.heap.ArenaAllocator.init(ctx.allocator);
        const arena_alloc = arena.allocator();
        const flavor: JsAdapter.Flavor = if (fhir_mode != 0) .fhir_json else .generic_json;
        var adapter = JsAdapter.init(arena_alloc, root_js_id, flavor);
        var eval_ctx = eval.EvalContext(JsAdapter){
            .allocator = arena_alloc,
            .adapter = &adapter,

            .schema = schema_ptr,
            .timestamp = ctx.now_epoch_seconds,
        };
        const items = eval.evalExpression(&eval_ctx, expr, adapter.root(), null) catch |err| {
            arena.deinit();
            return statusFromError(err);
        };
        ctx.result = eval.resolveResult(JsAdapter, &adapter, items, &arena, schema_ptr) catch |err| {
            arena.deinit();
            return statusFromError(err);
        };
    }
    ctx.last_schema = schema_ptr;
    return .ok;
}

pub export fn fhirpath_result_count(ctx_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    if (ctx.result == null) return 0;
    return @intCast(ctx.result.?.items.items.len);
}

pub export fn fhirpath_result_iter_new(ctx_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    ctx.iters.append(ctx.allocator, 0) catch return 0;
    return @intCast(ctx.iters.items.len);
}

pub export fn fhirpath_result_iter_next(ctx_handle: u32, iter_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    if (ctx.result == null) return 0;
    if (iter_handle == 0) return 0;
    const idx: usize = @intCast(iter_handle - 1);
    if (idx >= ctx.iters.items.len) return 0;
    const pos = ctx.iters.items[idx];
    if (pos >= ctx.result.?.items.items.len) return 0;
    ctx.iters.items[idx] = pos + 1;
    return @intCast(pos + 1);
}

pub export fn fhirpath_item_data_kind(ctx_handle: u32, item_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const it = itemFromHandle(ctx, item_handle) orelse return 0;
    return @intFromEnum(it.data_kind);
}

pub export fn fhirpath_item_value_kind(ctx_handle: u32, item_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const it = itemFromHandle(ctx, item_handle) orelse return 0;
    return @intFromEnum(it.value_kind);
}

pub export fn fhirpath_item_type_id(ctx_handle: u32, item_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const it = itemFromHandle(ctx, item_handle) orelse return 0;
    if (ctx.last_schema) |s| return s.outputTypeId(it.type_id);
    return it.type_id;
}

pub export fn fhirpath_type_name_ptr(ctx_handle: u32, type_id: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const name = typeNameSlice(ctx, type_id);
    if (name.len == 0) return 0;
    return @intCast(@intFromPtr(name.ptr));
}

pub export fn fhirpath_type_name_len(ctx_handle: u32, type_id: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const name = typeNameSlice(ctx, type_id);
    return @intCast(name.len);
}

pub export fn fhirpath_item_source_pos(ctx_handle: u32, item_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const it = itemFromHandle(ctx, item_handle) orelse return 0;
    return it.source_pos;
}

pub export fn fhirpath_item_source_end(ctx_handle: u32, item_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const it = itemFromHandle(ctx, item_handle) orelse return 0;
    return it.source_end;
}

pub export fn fhirpath_item_data_ptr(ctx_handle: u32, item_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const it = itemFromHandle(ctx, item_handle) orelse return 0;
    const slice = nodeTextSlice(ctx, it) orelse return 0;
    return @intCast(@intFromPtr(slice.ptr));
}

pub export fn fhirpath_item_data_len(ctx_handle: u32, item_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const it = itemFromHandle(ctx, item_handle) orelse return 0;
    const slice = nodeTextSlice(ctx, it) orelse return 0;
    return @intCast(slice.len);
}

pub export fn fhirpath_item_bool(ctx_handle: u32, item_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const it = itemFromHandle(ctx, item_handle) orelse return 0;
    if (it.data_kind == .value and it.value != null and it.value.? == .boolean) {
        return if (it.value.?.boolean) 1 else 0;
    }
    if (nodeJsonValue(ctx, it)) |jv| {
        if (jv == .bool) return if (jv.bool) 1 else 0;
    }
    return 0;
}

pub export fn fhirpath_item_i64(ctx_handle: u32, item_handle: u32) i64 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const it = itemFromHandle(ctx, item_handle) orelse return 0;
    if (it.data_kind == .value and it.value != null and it.value.? == .integer) {
        return it.value.?.integer;
    }
    if (nodeJsonValue(ctx, it)) |jv| {
        switch (jv) {
            .integer => |i| return i,
            .number_string => |s| return std.fmt.parseInt(i64, s, 10) catch 0,
            else => {},
        }
    }
    return 0;
}

pub export fn fhirpath_item_str_ptr(ctx_handle: u32, item_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const it = itemFromHandle(ctx, item_handle) orelse return 0;
    if (it.data_kind == .value and it.value != null) {
        const s = switch (it.value.?) {
            .string => |v| v,
            .date => |v| v,
            .time => |v| v,
            .dateTime => |v| v,
            .decimal => |v| v,
            else => return 0,
        };
        return @intCast(@intFromPtr(s.ptr));
    }
    if (nodeStringSlice(ctx, it)) |s| {
        return @intCast(@intFromPtr(s.ptr));
    }
    return 0;
}

pub export fn fhirpath_item_str_len(ctx_handle: u32, item_handle: u32) u32 {
    const ctx = ctxFromHandle(ctx_handle) orelse return 0;
    const it = itemFromHandle(ctx, item_handle) orelse return 0;
    if (it.data_kind == .value and it.value != null) {
        const s = switch (it.value.?) {
            .string => |v| v,
            .date => |v| v,
            .time => |v| v,
            .dateTime => |v| v,
            .decimal => |v| v,
            else => return 0,
        };
        return @intCast(s.len);
    }
    if (nodeStringSlice(ctx, it)) |s| {
        return @intCast(s.len);
    }
    return 0;
}

pub export fn fhirpath_item_decimal(ctx_handle: u32, item_handle: u32, out_ptr: u32) Status {
    if (out_ptr == 0) return .invalid_options;
    const ctx = ctxFromHandle(ctx_handle) orelse return .invalid_options;
    const it = itemFromHandle(ctx, item_handle) orelse return .invalid_options;
    const out = @as(*DecimalOut, @ptrFromInt(out_ptr));

    var text: ?[]const u8 = null;
    if (it.data_kind == .value and it.value != null) {
        if (it.value.? == .decimal) text = it.value.?.decimal;
        if (it.value.? == .integer) {
            var buf: [32]u8 = undefined;
            const s = std.fmt.bufPrint(&buf, "{d}", .{it.value.?.integer}) catch return .invalid_options;
            if (!parseDecimal(s, out)) return .unsupported;
            return .ok;
        }
    } else if (nodeStringSlice(ctx, it)) |s| {
        text = s;
    }

    if (text == null) return .invalid_options;
    if (!parseDecimal(text.?, out)) return .unsupported;
    return .ok;
}

pub export fn fhirpath_item_quantity(ctx_handle: u32, item_handle: u32, out_ptr: u32) Status {
    if (out_ptr == 0) return .invalid_options;
    const ctx = ctxFromHandle(ctx_handle) orelse return .invalid_options;
    const it = itemFromHandle(ctx, item_handle) orelse return .invalid_options;
    if (it.data_kind != .value or it.value == null or it.value.? != .quantity) return .invalid_options;

    const q = it.value.?.quantity;
    const out = @as(*QuantityOut, @ptrFromInt(out_ptr));
    if (!parseDecimal(q.value, &out.dec)) return .unsupported;
    out.unit_ptr = @intCast(@intFromPtr(q.unit.ptr));
    out.unit_len = @intCast(q.unit.len);
    return .ok;
}

