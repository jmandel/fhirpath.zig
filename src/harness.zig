const std = @import("std");
const lib = @import("lib.zig");
const eval = @import("eval.zig");
const ast = @import("ast.zig");

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

        var env_map: ?eval.Env = null;
        if (env_val) |e| {
            if (e == .object) {
                var env = eval.Env.init(allocator);
                var it = e.object.iterator();
                while (it.next()) |entry| {
                    try env.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                env_map = env;
            }
        }
        defer if (env_map) |*env| env.deinit();

        var result = eval.evalExpression(allocator, expr, input_val, if (env_map) |*env| env else null) catch {
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

        if (!compareExpected(expect_items, result.items)) {
            const expected_str = if (expect_val) |ev|
                try std.json.Stringify.valueAlloc(allocator, ev, .{})
            else
                try std.json.Stringify.valueAlloc(allocator, std.json.Value{ .array = std.json.Array{ .items = empty_items[0..], .capacity = 0, .allocator = allocator } }, .{});
            defer allocator.free(expected_str);

            const actual_array = std.json.Array{ .items = result.items, .capacity = result.capacity, .allocator = allocator };
            const actual_str = try std.json.Stringify.valueAlloc(allocator, std.json.Value{ .array = actual_array }, .{});
            defer allocator.free(actual_str);
            try reportCase(name, expr_val.string, "mismatch", expected_str, actual_str);
            failures += 1;
        }

        _ = idx;
    }

    if (failures > 0) {
        std.debug.print("\n{d} test(s) failed\n", .{failures});
        return error.TestFailed;
    }
    std.debug.print("All artisinal tests passed\n", .{});
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
