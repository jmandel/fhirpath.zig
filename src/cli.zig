const std = @import("std");
const lib = @import("lib.zig");
const eval = @import("eval.zig");
const ast = @import("ast.zig");
const JsonAdapter = @import("backends/json_adapter.zig").JsonAdapter;
const item = @import("item.zig");
const convert = @import("convert.zig");
const schema = @import("schema.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var typed_output = false;
    var model_path: ?[]const u8 = null;
    var expr_str: ?[]const u8 = null;
    var json_str: []const u8 = "{}";

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        } else if (std.mem.eql(u8, arg, "--typed") or std.mem.eql(u8, arg, "-t")) {
            typed_output = true;
        } else if (std.mem.eql(u8, arg, "--model") or std.mem.eql(u8, arg, "-m")) {
            i += 1;
            if (i < args.len) model_path = args[i];
        } else if (expr_str == null) {
            expr_str = arg;
        } else {
            json_str = arg;
        }
    }

    if (expr_str == null) {
        printUsage();
        return;
    }

    const expr = lib.parseExpression(allocator, expr_str.?) catch |err| {
        std.debug.print("Expression parse error: {}\n", .{err});
        return;
    };
    defer ast.deinitExpr(allocator, expr);

    var types = try item.TypeTable.init(allocator);
    defer types.deinit();

    var eval_arena = std.heap.ArenaAllocator.init(allocator);
    defer eval_arena.deinit();
    const arena_alloc = eval_arena.allocator();

    const parsed = std.json.parseFromSliceLeaky(std.json.Value, arena_alloc, json_str, .{ .parse_numbers = false }) catch |err| {
        std.debug.print("JSON parse error: {}\n", .{err});
        return;
    };

    // Load schema if specified
    var schema_obj: ?schema.Schema = null;
    var model_bytes: ?[]u8 = null;
    if (model_path) |path| {
        if (std.fs.cwd().readFileAlloc(allocator, path, 128 * 1024 * 1024)) |bytes| {
            model_bytes = bytes;
            if (schema.Schema.init(allocator, "default", "FHIR", bytes)) |s| {
                schema_obj = s;
            } else |err| {
                std.debug.print("Schema init error: {}\n", .{err});
            }
        } else |err| {
            std.debug.print("Model load error: {}\n", .{err});
        }
    }
    defer if (model_bytes) |bytes| allocator.free(bytes);
    defer if (schema_obj) |*s| s.deinit();

    const schema_p = if (schema_obj) |*s| s else null;
    const flavor: JsonAdapter.Flavor = if (schema_p != null) .fhir_json else .generic_json;
    const root_val = arena_alloc.create(std.json.Value) catch |err| {
        std.debug.print("Alloc error: {}\n", .{err});
        return;
    };
    root_val.* = parsed;
    var adapter = JsonAdapter.init(arena_alloc, root_val, flavor);
    defer adapter.deinit();
    var ctx = eval.EvalContext(JsonAdapter){
        .allocator = arena_alloc,
        .adapter = &adapter,
        .types = &types,
        .schema = schema_p,
        .timestamp = std.time.timestamp(),
    };
    var result = eval.evalExpression(&ctx, expr, adapter.root(), null) catch |err| {
        std.debug.print("Eval error: {}\n", .{err});
        return;
    };
    defer result.deinit(arena_alloc);

    var out_arr = std.ArrayList(std.json.Value).empty;
    defer out_arr.deinit(allocator);
    for (result.items) |it| {
        if (typed_output) {
            // Get type name from schema or TypeTable
            var type_name: []const u8 = "";
            if (schema.isModelType(it.type_id)) {
                if (schema_obj) |*s| {
                    type_name = s.typeName(it.type_id);
                }
            } else {
                type_name = types.name(it.type_id);
            }
            const val = try convert.adapterItemToTypedJsonValue(JsonAdapter, allocator, &adapter, it, type_name);
            try out_arr.append(allocator, val);
        } else {
            const val = try convert.adapterItemToJsonValue(JsonAdapter, allocator, &adapter, it);
            try out_arr.append(allocator, val);
        }
    }

    const output = std.json.Stringify.valueAlloc(
        allocator,
        std.json.Value{ .array = std.json.Array{ .items = out_arr.items, .capacity = out_arr.capacity, .allocator = allocator } },
        .{ .whitespace = .indent_2 },
    ) catch |err| {
        std.debug.print("Output error: {}\n", .{err});
        return;
    };
    defer allocator.free(output);
    std.debug.print("{s}\n", .{output});
}

fn printUsage() void {
    const usage =
        \\Usage: fhirpath [options] <expression> [json]
        \\
        \\Evaluate a FHIRPath expression against JSON input.
        \\
        \\Options:
        \\  -t, --typed       Output {type, value} objects
        \\  -m, --model PATH  Load FHIR model for type resolution
        \\
        \\Arguments:
        \\  expression  FHIRPath expression to evaluate
        \\  json        JSON input (default: {})
        \\
        \\Examples:
        \\  fhirpath 'name.given' '{"name":{"given":["Ann","Bob"]}}'
        \\  fhirpath -t 'true'   # outputs [{"type":"System.Boolean","value":true}]
        \\  fhirpath -t -m models/r5/model.bin 'identifier' '<patient-json>'
        \\
    ;
    std.debug.print("{s}", .{usage});
}
