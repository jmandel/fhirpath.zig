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

    const path = if (args.len > 1) args[1] else "tests/artisinal/basic.json";

    const data = try std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024);
    defer allocator.free(data);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, data, .{});
    defer parsed.deinit();

    const root = parsed.value;
    const cases_val = root.object.get("cases") orelse {
        std.debug.print("Missing cases array\n", .{});
        return error.InvalidFormat;
    };

    if (cases_val != .array) return error.InvalidFormat;

    var failures: usize = 0;
    var types = try item.TypeTable.init(allocator);
    defer types.deinit();

    for (cases_val.array.items, 0..) |case_val, idx| {
        if (case_val != .object) continue;
        const obj = case_val.object;
        const name = obj.get("name") orelse std.json.Value{ .string = "(unnamed)" };
        const expr_val = obj.get("expr") orelse {
            failures += 1;
            continue;
        };
        if (expr_val != .string) {
            failures += 1;
            continue;
        }

        const input_val = obj.get("input") orelse std.json.Value{ .null = {} };
        const env_val = obj.get("env");
        const expect_val = obj.get("expect");
        var empty_items = [_]std.json.Value{};
        const empty_expect: []const std.json.Value = empty_items[0..];
        var expect_items = empty_expect;

        const expr = lib.parseExpression(allocator, expr_val.string) catch {
            try reportCase(name, expr_val.string, "parse error", null, null);
            failures += 1;
            continue;
        };
        defer ast.deinitExpr(allocator, expr);

        const input_json = try std.json.Stringify.valueAlloc(allocator, input_val, .{});
        defer allocator.free(input_json);

        var env_map = eval.Env.init(allocator);
        defer env_map.deinit();
        var env_storage = std.ArrayList([]item.Item).empty;
        defer {
            for (env_storage.items) |slice| allocator.free(slice);
            env_storage.deinit(allocator);
        }
        var env_ptr: ?*eval.Env = null;

        var doc: jsondoc.JsonDoc = undefined;
        var doc_inited = false;
        defer if (doc_inited) doc.deinit();

        var root_idx: jsondoc.NodeIndex = 0;

        if (env_val) |ev| {
            const env_json = try std.json.Stringify.valueAlloc(allocator, ev, .{});
            defer allocator.free(env_json);
            const combined = try std.fmt.allocPrint(allocator, "{{\"__input\":{s},\"__env\":{s}}}", .{ input_json, env_json });
            defer allocator.free(combined);
            doc = jsondoc.JsonDoc.init(allocator, combined) catch {
                try reportCase(name, expr_val.string, "json parse error", null, null);
                failures += 1;
                continue;
            };
            doc_inited = true;
            root_idx = objectField(&doc, doc.root, "__input") orelse doc.root;
            if (objectField(&doc, doc.root, "__env")) |env_idx| {
                const env_node = doc.node(env_idx).*;
                if (env_node.kind == .object) {
                    for (env_node.data.object) |field| {
                        var slice = try allocator.alloc(item.Item, 1);
                        slice[0] = makeNodeItem(&doc, field.value);
                        try env_storage.append(allocator, slice);
                        try env_map.put(field.key, slice);
                        env_ptr = &env_map;
                    }
                }
            }
        } else {
            doc = jsondoc.JsonDoc.init(allocator, input_json) catch {
                try reportCase(name, expr_val.string, "json parse error", null, null);
                failures += 1;
                continue;
            };
            doc_inited = true;
            root_idx = doc.root;
        }

        var ctx = eval.EvalContext{ .allocator = allocator, .doc = &doc, .types = &types, .schema = null };
        var result = eval.evalExpression(&ctx, expr, root_idx, env_ptr) catch {
            try reportCase(name, expr_val.string, "eval error", null, null);
            failures += 1;
            continue;
        };
        defer result.deinit(allocator);

        if (expect_val) |ev| {
            if (ev != .array) {
                failures += 1;
                continue;
            }
            expect_items = ev.array.items;
        }

        var actual_values = try itemsToJsonArray(allocator, &doc, result.items);
        defer actual_values.deinit(allocator);

        const matched = compareExpected(expect_items, actual_values.items);
        if (!matched) {
            const expected_str = if (expect_val) |ev|
                try std.json.Stringify.valueAlloc(allocator, ev, .{})
            else
                try std.json.Stringify.valueAlloc(allocator, std.json.Value{ .array = std.json.Array{ .items = empty_items[0..], .capacity = 0, .allocator = allocator } }, .{});
            defer allocator.free(expected_str);

            const actual_array = std.json.Array{ .items = actual_values.items, .capacity = actual_values.capacity, .allocator = allocator };
            const actual_str = try std.json.Stringify.valueAlloc(allocator, std.json.Value{ .array = actual_array }, .{});
            defer allocator.free(actual_str);
            try reportCase(name, expr_val.string, "mismatch", expected_str, actual_str);
            failures += 1;
        }
        for (actual_values.items) |val| {
            deinitValue(allocator, val);
        }

        _ = idx;
    }

    if (failures > 0) {
        std.debug.print("\n{d} test(s) failed\n", .{failures});
        return error.TestFailed;
    }
    std.debug.print("All artisinal tests passed\n", .{});
}

fn itemsToJsonArray(
    allocator: std.mem.Allocator,
    doc: *jsondoc.JsonDoc,
    items: []const item.Item,
) !std.ArrayList(std.json.Value) {
    var out = std.ArrayList(std.json.Value).empty;
    for (items) |it| {
        const val = try convert.itemToJsonValue(allocator, doc, it);
        try out.append(allocator, val);
    }
    return out;
}

fn compareExpected(expected: []const std.json.Value, actual: []const std.json.Value) bool {
    if (expected.len != actual.len) return false;
    var i: usize = 0;
    while (i < expected.len) : (i += 1) {
        if (!jsonEqual(expected[i], actual[i])) return false;
    }
    return true;
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
            for (arr.items, 0..) |item_val, idx| {
                if (!jsonEqual(item_val, arr_b.items[idx])) return false;
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

fn reportCase(name_val: std.json.Value, expr: []const u8, msg: []const u8, expected: ?[]const u8, actual: ?[]const u8) !void {
    if (name_val == .string) {
        std.debug.print("[FAIL] {s}: {s}\n", .{ name_val.string, msg });
    } else {
        std.debug.print("[FAIL] {s}\n", .{ msg });
    }
    std.debug.print("  expr: {s}\n", .{expr});
    if (expected) |e| std.debug.print("  expected: {s}\n", .{e});
    if (actual) |a| std.debug.print("  actual:   {s}\n", .{a});
}

fn objectField(doc: *jsondoc.JsonDoc, obj_idx: jsondoc.NodeIndex, key: []const u8) ?jsondoc.NodeIndex {
    const node = doc.node(obj_idx).*;
    if (node.kind != .object) return null;
    for (node.data.object) |field| {
        if (std.mem.eql(u8, field.key, key)) return field.value;
    }
    return null;
}

fn makeNodeItem(doc: *jsondoc.JsonDoc, node_idx: jsondoc.NodeIndex) item.Item {
    const node = doc.node(node_idx).*;
    return .{
        .data_kind = .json_span,
        .value_kind = .empty,
        .type_id = 0,
        .source_pos = 0,
        .source_end = 0,
        .data_pos = node.start,
        .data_end = node.end,
        .node = node_idx,
        .value = null,
    };
}

fn deinitValue(allocator: std.mem.Allocator, v: std.json.Value) void {
    switch (v) {
        .array => |arr| {
            for (arr.items) |item_val| {
                deinitValue(allocator, item_val);
            }
            allocator.free(arr.items);
        },
        .object => |obj| {
            var obj_mut = obj;
            var it = obj_mut.iterator();
            while (it.next()) |entry| {
                deinitValue(allocator, entry.value_ptr.*);
            }
            obj_mut.deinit();
        },
        else => {},
    }
}
