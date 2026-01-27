const std = @import("std");
const lib = @import("lib.zig");
const eval = @import("eval.zig");
const ast = @import("ast.zig");
const jsondoc = @import("jsondoc.zig");
const item = @import("item.zig");
const convert = @import("convert.zig");

const Options = struct {
    show_failures: bool = true,
    max_failures: usize = 0, // 0 = all
    filter: ?[]const u8 = null,
    verbose: bool = false,
};

const FileResult = struct {
    name: []const u8,
    passed: usize,
    failed: usize,
    parse_errors: usize,
    eval_errors: usize,
    mismatch_errors: usize,
};

const TestFailure = struct {
    file: []const u8,
    name: []const u8,
    expr: []const u8,
    reason: []const u8,
    expected: ?[]const u8,
    actual: ?[]const u8,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var opts = Options{};
    var specific_file: ?[]const u8 = null;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        } else if (std.mem.eql(u8, arg, "--no-failures") or std.mem.eql(u8, arg, "-q")) {
            opts.show_failures = false;
        } else if (std.mem.eql(u8, arg, "--max") or std.mem.eql(u8, arg, "-n")) {
            i += 1;
            if (i < args.len) {
                opts.max_failures = std.fmt.parseInt(usize, args[i], 10) catch 10;
            }
        } else if (std.mem.eql(u8, arg, "--filter") or std.mem.eql(u8, arg, "-f")) {
            i += 1;
            if (i < args.len) opts.filter = args[i];
        } else if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            opts.verbose = true;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            specific_file = arg;
        }
    }

    var results = std.ArrayList(FileResult).empty;
    defer {
        for (results.items) |r| allocator.free(r.name);
        results.deinit(allocator);
    }

    var failures = std.ArrayList(TestFailure).empty;
    defer {
        for (failures.items) |f| {
            allocator.free(f.file);
            allocator.free(f.name);
            allocator.free(f.expr);
            allocator.free(f.reason);
            if (f.expected) |e| allocator.free(e);
            if (f.actual) |a| allocator.free(a);
        }
        failures.deinit(allocator);
    }

    if (specific_file) |path| {
        const result = try runTestFile(allocator, path, &failures, opts);
        try results.append(allocator, result);
    } else {
        var dir = std.fs.cwd().openDir("tests/artisinal", .{ .iterate = true }) catch |err| {
            std.debug.print("Cannot open tests/artisinal: {}\n", .{err});
            return err;
        };
        defer dir.close();

        var iter = dir.iterate();
        while (try iter.next()) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.name, ".json")) continue;
            if (opts.filter) |f| {
                if (std.mem.indexOf(u8, entry.name, f) == null) continue;
            }

            const full_path = try std.fmt.allocPrint(allocator, "tests/artisinal/{s}", .{entry.name});
            defer allocator.free(full_path);

            const result = try runTestFile(allocator, full_path, &failures, opts);
            try results.append(allocator, result);
        }

        // Sort by filename
        std.mem.sort(FileResult, results.items, {}, struct {
            fn lessThan(_: void, a: FileResult, b: FileResult) bool {
                return std.mem.lessThan(u8, a.name, b.name);
            }
        }.lessThan);
    }

    // Print failures
    if (opts.show_failures and failures.items.len > 0) {
        const show_count = if (opts.max_failures > 0)
            @min(opts.max_failures, failures.items.len)
        else
            failures.items.len;

        std.debug.print("\n=== FAILURES ({d}/{d} shown) ===\n", .{ show_count, failures.items.len });
        for (failures.items[0..show_count]) |f| {
            std.debug.print("\n[FAIL] {s}:{s}\n", .{ f.file, f.name });
            std.debug.print("  expr: {s}\n", .{f.expr});
            std.debug.print("  reason: {s}\n", .{f.reason});
            if (f.expected) |e| std.debug.print("  expected: {s}\n", .{e});
            if (f.actual) |a| std.debug.print("  actual:   {s}\n", .{a});
        }
        if (show_count < failures.items.len) {
            std.debug.print("\n... and {d} more failures (use --max 0 to show all)\n", .{failures.items.len - show_count});
        }
    }

    // Print summary
    printSummary(results.items);

    var total_failed: usize = 0;
    for (results.items) |r| total_failed += r.failed;
    if (total_failed > 0) return error.TestFailed;
}

fn printUsage() void {
    const usage =
        \\Usage: harness [options] [file.json]
        \\
        \\Run artisinal FHIRPath tests.
        \\
        \\Options:
        \\  -q, --no-failures   Don't show failure details (summary only)
        \\  -n, --max N         Show at most N failures (default: all)
        \\  -f, --filter PATTERN Filter files/tests by pattern
        \\  -v, --verbose       Show passing tests too
        \\  -h, --help          Show this help
        \\
        \\If no file is specified, runs all tests/artisinal/*.json
        \\
    ;
    std.debug.print("{s}", .{usage});
}

fn printSummary(results: []const FileResult) void {
    var total_passed: usize = 0;
    var total_failed: usize = 0;
    var total_parse: usize = 0;
    var total_eval: usize = 0;
    var total_mismatch: usize = 0;

    std.debug.print("\n{s:<40} {s:>10} {s:>7} {s:>7} {s:>7}\n", .{ "File", "Pass/Total", "Parse", "Eval", "Match" });
    std.debug.print("{s}\n", .{"-" ** 75});

    for (results) |r| {
        const total = r.passed + r.failed;
        std.debug.print("{s:<40} {d:>4}/{d:<5} {d:>7} {d:>7} {d:>7}\n", .{
            r.name,
            r.passed,
            total,
            r.parse_errors,
            r.eval_errors,
            r.mismatch_errors,
        });
        total_passed += r.passed;
        total_failed += r.failed;
        total_parse += r.parse_errors;
        total_eval += r.eval_errors;
        total_mismatch += r.mismatch_errors;
    }

    std.debug.print("{s}\n", .{"-" ** 75});
    const grand_total = total_passed + total_failed;
    const pct = if (grand_total > 0) (total_passed * 100) / grand_total else 0;
    std.debug.print("{s:<40} {d:>4}/{d:<5} {d:>7} {d:>7} {d:>7}\n", .{
        "TOTAL",
        total_passed,
        grand_total,
        total_parse,
        total_eval,
        total_mismatch,
    });
    std.debug.print("\nPass rate: {d}% ({d}/{d})\n", .{ pct, total_passed, grand_total });
}

fn runTestFile(
    allocator: std.mem.Allocator,
    path: []const u8,
    failures: *std.ArrayList(TestFailure),
    opts: Options,
) !FileResult {
    var result = FileResult{
        .name = try allocator.dupe(u8, std.fs.path.basename(path)),
        .passed = 0,
        .failed = 0,
        .parse_errors = 0,
        .eval_errors = 0,
        .mismatch_errors = 0,
    };

    const data = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Cannot read {s}: {}\n", .{ path, err });
        return result;
    };
    defer allocator.free(data);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, data, .{}) catch {
        result.failed = 1;
        result.parse_errors = 1;
        return result;
    };
    defer parsed.deinit();

    const root = parsed.value;
    const cases_val = root.object.get("cases") orelse return result;
    if (cases_val != .array) return result;

    var types = try item.TypeTable.init(allocator);
    defer types.deinit();

    for (cases_val.array.items) |case_val| {
        if (case_val != .object) continue;
        const obj = case_val.object;
        const name_val = obj.get("name") orelse std.json.Value{ .string = "(unnamed)" };
        const name_str = if (name_val == .string) name_val.string else "(unnamed)";

        // Apply filter
        if (opts.filter) |f| {
            if (std.mem.indexOf(u8, name_str, f) == null and
                std.mem.indexOf(u8, result.name, f) == null)
            {
                continue;
            }
        }

        const expr_val = obj.get("expr") orelse {
            result.failed += 1;
            continue;
        };
        if (expr_val != .string) {
            result.failed += 1;
            continue;
        }
        const expr_str = expr_val.string;

        const input_val = obj.get("input") orelse std.json.Value{ .null = {} };
        const input_str = std.json.Stringify.valueAlloc(allocator, input_val, .{}) catch "{}";
        defer allocator.free(input_str);

        const expr = lib.parseExpression(allocator, expr_str) catch {
            result.failed += 1;
            result.parse_errors += 1;
            try addFailure(allocator, failures, result.name, name_str, expr_str, "parse error", null, null);
            continue;
        };
        defer ast.deinitExpr(allocator, expr);

        var doc = jsondoc.JsonDoc.init(allocator, input_str) catch {
            result.failed += 1;
            result.parse_errors += 1;
            try addFailure(allocator, failures, result.name, name_str, expr_str, "json parse error", null, null);
            continue;
        };
        defer doc.deinit();

        var ctx = eval.EvalContext{ .allocator = allocator, .doc = &doc, .types = &types, .schema = null };
        var eval_result = eval.evalExpression(&ctx, expr, doc.root, null) catch {
            result.failed += 1;
            result.eval_errors += 1;
            try addFailure(allocator, failures, result.name, name_str, expr_str, "eval error", null, null);
            continue;
        };
        defer eval_result.deinit(allocator);

        const expect_val = obj.get("expect");
        const empty_items: []const std.json.Value = &[_]std.json.Value{};
        var expect_items = empty_items;
        if (expect_val) |ev| {
            if (ev == .array) {
                expect_items = ev.array.items;
            }
        }

        var actual_values = try itemsToJsonArray(allocator, &doc, eval_result.items, &types);
        defer actual_values.deinit(allocator);

        const matched = compareExpected(expect_items, actual_values.items);
        if (!matched) {
            result.failed += 1;
            result.mismatch_errors += 1;

            const expected_str = if (expect_val) |ev|
                std.json.Stringify.valueAlloc(allocator, ev, .{}) catch "?"
            else
                allocator.dupe(u8, "[]") catch "?";

            const actual_array = std.json.Array{ .items = actual_values.items, .capacity = actual_values.capacity, .allocator = allocator };
            const actual_str = std.json.Stringify.valueAlloc(allocator, std.json.Value{ .array = actual_array }, .{}) catch "?";

            try addFailure(allocator, failures, result.name, name_str, expr_str, "mismatch", expected_str, actual_str);
        } else {
            result.passed += 1;
            if (opts.verbose) {
                std.debug.print("[PASS] {s}:{s}\n", .{ result.name, name_str });
            }
        }

        for (actual_values.items) |val| {
            deinitValue(allocator, val);
        }
    }

    return result;
}

fn addFailure(
    allocator: std.mem.Allocator,
    failures: *std.ArrayList(TestFailure),
    file: []const u8,
    name: []const u8,
    expr: []const u8,
    reason: []const u8,
    expected: ?[]const u8,
    actual: ?[]const u8,
) !void {
    try failures.append(allocator, .{
        .file = try allocator.dupe(u8, file),
        .name = try allocator.dupe(u8, name),
        .expr = try allocator.dupe(u8, expr),
        .reason = try allocator.dupe(u8, reason),
        .expected = if (expected) |e| try allocator.dupe(u8, e) else null,
        .actual = if (actual) |a| try allocator.dupe(u8, a) else null,
    });
}

fn itemsToJsonArray(
    allocator: std.mem.Allocator,
    doc: *jsondoc.JsonDoc,
    items: []const item.Item,
    types: *item.TypeTable,
) !std.ArrayList(std.json.Value) {
    var out = std.ArrayList(std.json.Value).empty;
    for (items) |it| {
        const type_name = types.name(it.type_id);
        const val = try convert.itemToTypedJsonValue(allocator, doc, it, type_name);
        try out.append(allocator, val);
    }
    return out;
}

fn compareExpected(expected: []const std.json.Value, actual: []const std.json.Value) bool {
    if (expected.len != actual.len) return false;
    var i: usize = 0;
    while (i < expected.len) : (i += 1) {
        if (!compareTypedValue(expected[i], actual[i])) return false;
    }
    return true;
}

fn compareTypedValue(expected: std.json.Value, actual: std.json.Value) bool {
    if (expected != .object or actual != .object) {
        return jsonEqual(expected, actual);
    }

    const exp_obj = expected.object;
    const act_obj = actual.object;

    const exp_type = exp_obj.get("type") orelse return false;
    const act_type = act_obj.get("type") orelse return false;
    if (exp_type != .string or act_type != .string) return false;

    if (!typesMatch(exp_type.string, act_type.string)) return false;

    const exp_val = exp_obj.get("value") orelse return false;
    const act_val = act_obj.get("value") orelse return false;
    return jsonEqual(exp_val, act_val);
}

fn typesMatch(expected: []const u8, actual: []const u8) bool {
    if (std.mem.eql(u8, expected, actual)) return true;

    if (std.mem.indexOfScalar(u8, expected, '.') == null) {
        if (std.mem.lastIndexOfScalar(u8, actual, '.')) |dot_pos| {
            const actual_suffix = actual[dot_pos + 1 ..];
            if (std.ascii.eqlIgnoreCase(expected, actual_suffix)) return true;
        }
    }

    return false;
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
                const val_b = obj_b.get(entry.key_ptr.*) orelse return false;
                if (!jsonEqual(entry.value_ptr.*, val_b)) return false;
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

fn deinitValue(allocator: std.mem.Allocator, v: std.json.Value) void {
    switch (v) {
        .array => |arr| {
            for (arr.items) |child| {
                deinitValue(allocator, child);
            }
            allocator.free(arr.items);
        },
        .object => |*obj| {
            var iter = obj.iterator();
            while (iter.next()) |entry| {
                deinitValue(allocator, entry.value_ptr.*);
            }
            @constCast(obj).deinit();
        },
        else => {},
    }
}
