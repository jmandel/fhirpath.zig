const std = @import("std");
const lib = @import("lib.zig");
const eval = @import("eval.zig");
const ast = @import("ast.zig");
const jsondoc = @import("jsondoc.zig");
const item = @import("item.zig");
const convert = @import("convert.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2 or std.mem.eql(u8, args[1], "--help") or std.mem.eql(u8, args[1], "-h")) {
        printUsage();
        return;
    }

    const expr_str = args[1];
    const json_str = if (args.len > 2) args[2] else "{}";

    const expr = lib.parseExpression(allocator, expr_str) catch |err| {
        std.debug.print("Expression parse error: {}\n", .{err});
        return;
    };
    defer ast.deinitExpr(allocator, expr);

    var types = try item.TypeTable.init(allocator);
    defer types.deinit();

    var doc = jsondoc.JsonDoc.init(allocator, json_str) catch |err| {
        std.debug.print("JSON parse error: {}\n", .{err});
        return;
    };
    defer doc.deinit();

    var ctx = eval.EvalContext{ .allocator = allocator, .doc = &doc, .types = &types, .schema = null };
    var result = eval.evalExpression(&ctx, expr, doc.root, null) catch |err| {
        std.debug.print("Eval error: {}\n", .{err});
        return;
    };
    defer result.deinit(allocator);

    var out_arr = std.ArrayList(std.json.Value).empty;
    defer out_arr.deinit(allocator);
    for (result.items) |it| {
        const val = try convert.itemToJsonValue(allocator, &doc, it);
        try out_arr.append(allocator, val);
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
        \\Usage: fhirpath <expression> [json]
        \\ 
        \\Evaluate a FHIRPath expression against JSON input.
        \\ 
        \\Arguments:
        \\  expression  FHIRPath expression to evaluate
        \\  json        JSON input (default: {})
        \\ 
        \\Examples:
        \\  fhirpath 'name.given' '{"name":{"given":["Ann","Bob"]}}'
        \\  fhirpath 'name.where(use = "official")' '{"name":[{"use":"official","given":["Ann"]}]}'
        \\  fhirpath 'items.count()' '{"items":[1,2,3]}'
        \\ 
    ;
    std.debug.print("{s}", .{usage});
}
