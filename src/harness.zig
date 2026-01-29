const std = @import("std");
const lib = @import("lib.zig");
const eval = @import("eval.zig");
const ast = @import("ast.zig");
const StdJsonAdapter = @import("backends/stdjson.zig").StdJsonAdapter;
const FhirJsonAdapter = @import("backends/fhirjson.zig").FhirJsonAdapter;
const item = @import("item.zig");
const convert = @import("convert.zig");
const schema = @import("schema.zig");

const Options = struct {
    show_failures: bool = true,
    max_failures: usize = 0,
    filter_file: ?[]const u8 = null,
    filter_test: ?[]const u8 = null,
    verbose: bool = false,
    model_path: ?[]const u8 = null,
    input_dir: ?[]const u8 = null,
    fhir_json: bool = false,
};

const FileResult = struct {
    name: []const u8,
    passed: usize,
    failed: usize,
    skipped: usize,
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

const OwnedStr = struct {
    buf: []const u8,
    owned: bool,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .verbose_log = false }){};
    defer {
        const result = gpa.deinit();
        if (result == .leak) {
            std.debug.print("\n=== MEMORY LEAK DETECTED ===\n", .{});
        }
    }
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var opts = Options{};
    var files = std.ArrayList([]const u8).empty;
    defer files.deinit(allocator);

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
            if (i < args.len) opts.max_failures = std.fmt.parseInt(usize, args[i], 10) catch 10;
        } else if (std.mem.eql(u8, arg, "--filter-file") or std.mem.eql(u8, arg, "-F")) {
            i += 1;
            if (i < args.len) opts.filter_file = args[i];
        } else if (std.mem.eql(u8, arg, "--filter-test") or std.mem.eql(u8, arg, "-t")) {
            i += 1;
            if (i < args.len) opts.filter_test = args[i];
        } else if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            opts.verbose = true;
        } else if (std.mem.eql(u8, arg, "--model") or std.mem.eql(u8, arg, "-m")) {
            i += 1;
            if (i < args.len) opts.model_path = args[i];
        } else if (std.mem.eql(u8, arg, "--input-dir") or std.mem.eql(u8, arg, "-i")) {
            i += 1;
            if (i < args.len) opts.input_dir = args[i];
        } else if (std.mem.eql(u8, arg, "--fhir-json")) {
            opts.fhir_json = true;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            try files.append(allocator, arg);
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

    // Load schema if specified
    var schema_obj: ?schema.Schema = null;
    var model_bytes: ?[]u8 = null;
    if (opts.model_path) |path| {
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

    if (files.items.len > 0) {
        // Run specified files
        for (files.items) |path| {
            const result = try runTestFile(allocator, path, &failures, opts, if (schema_obj) |*s| s else null);
            try results.append(allocator, result);
        }
    } else {
        // Default: run all artisinal tests
        var dir = std.fs.cwd().openDir("tests/artisinal", .{ .iterate = true }) catch |err| {
            std.debug.print("Cannot open tests/artisinal: {}\n", .{err});
            return err;
        };
        defer dir.close();

        var iter = dir.iterate();
        while (try iter.next()) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.name, ".json")) continue;
            if (opts.filter_file) |f| {
                if (std.mem.indexOf(u8, entry.name, f) == null) continue;
            }

            const full_path = try std.fmt.allocPrint(allocator, "tests/artisinal/{s}", .{entry.name});
            defer allocator.free(full_path);

            const result = try runTestFile(allocator, full_path, &failures, opts, if (schema_obj) |*s| s else null);
            try results.append(allocator, result);
        }

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

    printSummary(results.items);

    var total_failed: usize = 0;
    for (results.items) |r| total_failed += r.failed;
    if (total_failed > 0) return error.TestFailed;
}

fn printUsage() void {
    const usage =
        \\Usage: harness [options] [file.json ...]
        \\
        \\Run FHIRPath tests (artisinal or official r5 format).
        \\
        \\Options:
        \\  -F, --filter-file PATTERN  Filter by filename (substring match)
        \\  -t, --filter-test PATTERN  Filter by test name (substring match)
        \\  -n, --max N                Show at most N failures (default: all)
        \\  -q, --no-failures          Don't show failure details
        \\  -v, --verbose              Show passing tests too
        \\  -m, --model PATH           Load FHIR model for type resolution
        \\  --fhir-json                Use FHIR JSON adapter (merges _field primitives)
        \\  -i, --input-dir DIR        Directory for inputfile references
        \\  -h, --help                 Show this help
        \\
        \\If no files specified, runs all tests/artisinal/*.json
        \\
        \\Examples:
        \\  harness                              # all artisinal tests
        \\  harness -F string                    # only string-*.json files
        \\  harness -t substring                 # only tests with "substring" in name
        \\  harness -F math -t divide            # math files, divide tests
        \\  harness tests/r5/tests-fhir-r5.json  # official r5 tests
        \\
    ;
    std.debug.print("{s}", .{usage});
}

fn printSummary(results: []const FileResult) void {
    var total_passed: usize = 0;
    var total_failed: usize = 0;
    var total_skipped: usize = 0;
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
        total_skipped += r.skipped;
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
    if (total_skipped > 0) {
        std.debug.print("Skipped: {d}\n", .{total_skipped});
    }
    std.debug.print("\nPass rate: {d}% ({d}/{d})\n", .{ pct, total_passed, grand_total });
}

fn runTestFile(
    allocator: std.mem.Allocator,
    path: []const u8,
    failures: *std.ArrayList(TestFailure),
    opts: Options,
    schema_ptr: ?*schema.Schema,
) !FileResult {
    var result = FileResult{
        .name = try allocator.dupe(u8, std.fs.path.basename(path)),
        .passed = 0,
        .failed = 0,
        .skipped = 0,
        .parse_errors = 0,
        .eval_errors = 0,
        .mismatch_errors = 0,
    };

    const data = std.fs.cwd().readFileAlloc(allocator, path, 50 * 1024 * 1024) catch |err| {
        std.debug.print("Cannot read {s}: {}\n", .{ path, err });
        return result;
    };
    defer allocator.free(data);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, data, .{ .parse_numbers = false }) catch {
        result.failed = 1;
        result.parse_errors = 1;
        return result;
    };
    defer parsed.deinit();

    const root = parsed.value;

    // Auto-detect --fhir-json from meta.requires_fhir_json in test file
    var use_fhir_json = opts.fhir_json;
    if (!use_fhir_json) {
        if (root.object.get("meta")) |meta_val| {
            if (meta_val == .object) {
                if (meta_val.object.get("requires_fhir_json")) |rfj| {
                    if (rfj == .bool and rfj.bool) {
                        use_fhir_json = true;
                    }
                }
            }
        }
    }

    // Detect format: "cases" = artisinal, "tests" = r5 official
    const cases_key = if (root.object.get("cases") != null) "cases" else "tests";
    const cases_val = root.object.get(cases_key) orelse return result;
    if (cases_val != .array) return result;

    // Detect field names
    const is_official = std.mem.eql(u8, cases_key, "tests");
    const expr_key = if (is_official) "expression" else "expr";
    const expect_key = if (is_official) "outputs" else "expect";

    // Infer input_dir from path if not specified
    var input_dir = opts.input_dir;
    var input_dir_owned: ?[]const u8 = null;
    defer if (input_dir_owned) |d| allocator.free(d);
    if (input_dir == null and is_official) {
        // Default to sibling "input" directory
        const dir = std.fs.path.dirname(path) orelse ".";
        const inferred = std.fmt.allocPrint(allocator, "{s}/input", .{dir}) catch null;
        if (inferred) |d| {
            if (std.fs.cwd().access(d, .{})) |_| {
                input_dir = d;
                input_dir_owned = d;
            } else |_| {
                allocator.free(d);
            }
        }
    }

    var types = try item.TypeTable.init(allocator);
    defer types.deinit();

    for (cases_val.array.items) |case_val| {
        if (case_val != .object) continue;
        const obj = case_val.object;
        
        const name_val = obj.get("name") orelse std.json.Value{ .string = "(unnamed)" };
        const name_str = if (name_val == .string) name_val.string else "(unnamed)";

        // Apply test name filter
        if (opts.filter_test) |f| {
            if (std.mem.indexOf(u8, name_str, f) == null) continue;
        }

        // Skip tests with predicate flag (not supported)
        if (obj.get("predicate")) |pred| {
            if (pred == .bool and pred.bool) {
                result.skipped += 1;
                continue;
            }
        }

        // Check for skip marker (with optional reason string)
        if (obj.get("skip")) |skip_val| {
            if (skip_val == .string or (skip_val == .bool and skip_val.bool)) {
                result.skipped += 1;
                continue;
            }
        }

        // Check for expect_error marker
        const expect_error = if (obj.get("expect_error")) |inv| inv == .bool and inv.bool else false;

        const expr_val = obj.get(expr_key) orelse {
            // Skip section header entries in artisinal tests (comment-only or section-only)
            if (obj.get("_comment") != null or obj.get("_section") != null) {
                continue;
            }
            result.failed += 1;
            continue;
        };
        if (expr_val != .string) {
            result.failed += 1;
            continue;
        }
        const expr_str = expr_val.string;

        // Get input JSON
        var input_str: []const u8 = "{}";
        var input_allocated = false;
        defer if (input_allocated) allocator.free(input_str);

        if (obj.get("input")) |input_val| {
            input_str = std.json.Stringify.valueAlloc(allocator, input_val, .{}) catch "{}";
            input_allocated = true;
        } else if (obj.get("inputfile")) |inputfile_val| {
            if (inputfile_val == .string and inputfile_val.string.len > 0) {
                const filename = inputfile_val.string;
                // Convert .xml to .json
                const json_filename = if (std.mem.endsWith(u8, filename, ".xml"))
                    std.fmt.allocPrint(allocator, "{s}.json", .{filename[0 .. filename.len - 4]}) catch filename
                else
                    filename;
                defer if (json_filename.ptr != filename.ptr) allocator.free(json_filename);

                const full_input_path = if (input_dir) |dir|
                    std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir, json_filename }) catch json_filename
                else
                    json_filename;
                defer if (full_input_path.ptr != json_filename.ptr) allocator.free(full_input_path);

                if (std.fs.cwd().readFileAlloc(allocator, full_input_path, 10 * 1024 * 1024)) |contents| {
                    input_str = contents;
                    input_allocated = true;
                } else |_| {
                    // Input file not found - skip test
                    result.skipped += 1;
                    continue;
                }
            }
        }

        const expr = lib.parseExpression(allocator, expr_str) catch {
            if (expect_error) {
                result.passed += 1;
                if (opts.verbose) {
                    std.debug.print("[PASS] {s}:{s} (expected parse error)\n", .{ result.name, name_str });
                }
            } else {
                result.failed += 1;
                result.parse_errors += 1;
                try addFailure(allocator, failures, result.name, name_str, expr_str, "parse error", null, null);
            }
            continue;
        };
        defer ast.deinitExpr(allocator, expr);

        if (use_fhir_json) {
            // === FHIR JSON adapter path ===
            var std_parsed = std.json.parseFromSlice(std.json.Value, allocator, input_str, .{ .parse_numbers = false }) catch {
                if (expect_error) {
                    result.passed += 1;
                    if (opts.verbose) {
                        std.debug.print("[PASS] {s}:{s} (expected json error)\n", .{ result.name, name_str });
                    }
                } else {
                    result.failed += 1;
                    result.parse_errors += 1;
                    try addFailure(allocator, failures, result.name, name_str, expr_str, "json parse error", null, null);
                }
                continue;
            };
            defer std_parsed.deinit();

            var fhir_arena = std.heap.ArenaAllocator.init(allocator);
            defer fhir_arena.deinit();
            const arena_alloc = fhir_arena.allocator();

            var fhir_adapter = FhirJsonAdapter.init(arena_alloc, &std_parsed.value);
            fhir_adapter.schema = schema_ptr;
            defer fhir_adapter.deinit();

            var fhir_ctx = eval.EvalContext(FhirJsonAdapter){
                .allocator = arena_alloc,
                .adapter = &fhir_adapter,
                .types = &types,
                .schema = schema_ptr,
                .timestamp = std.time.timestamp(),
            };
            var fhir_eval_result = eval.evalExpression(&fhir_ctx, expr, fhir_adapter.root(), null) catch {
                if (expect_error) {
                    result.passed += 1;
                    if (opts.verbose) {
                        std.debug.print("[PASS] {s}:{s} (expected eval error)\n", .{ result.name, name_str });
                    }
                } else {
                    result.failed += 1;
                    result.eval_errors += 1;
                    try addFailure(allocator, failures, result.name, name_str, expr_str, "eval error", null, null);
                }
                continue;
            };

            if (expect_error) {
                result.failed += 1;
                try addFailure(allocator, failures, result.name, name_str, expr_str, "expected error but succeeded", null, null);
                fhir_eval_result.deinit(arena_alloc);
                continue;
            }
            defer fhir_eval_result.deinit(arena_alloc);

            const expect_val = obj.get(expect_key);
            const empty_items: []const std.json.Value = &[_]std.json.Value{};
            var expect_items = empty_items;
            if (expect_val) |ev| {
                if (ev == .array) {
                    expect_items = ev.array.items;
                }
            }

            var actual_values = try fhirItemsToJsonArray(allocator, &fhir_adapter, fhir_eval_result.items, &types, schema_ptr);
            defer actual_values.deinit(allocator);

            const unordered = if (obj.get("unordered")) |uv| uv == .bool and uv.bool else false;
            const matched = compareExpected(expect_items, actual_values.items, unordered);
            if (!matched) {
                result.failed += 1;
                result.mismatch_errors += 1;

                const expected_str = if (expect_val) |ev|
                    jsonStringifyOwned(allocator, ev)
                else
                    dupOwned(allocator, "[]");
                defer if (expected_str.owned) allocator.free(expected_str.buf);

                const actual_array = std.json.Array{ .items = actual_values.items, .capacity = actual_values.capacity, .allocator = allocator };
                const actual_str = jsonStringifyOwned(allocator, std.json.Value{ .array = actual_array });
                defer if (actual_str.owned) allocator.free(actual_str.buf);

                try addFailure(allocator, failures, result.name, name_str, expr_str, "mismatch", expected_str.buf, actual_str.buf);
            } else {
                result.passed += 1;
                if (opts.verbose) {
                    std.debug.print("[PASS] {s}:{s}\n", .{ result.name, name_str });
                }
            }

            for (actual_values.items) |val| {
                deinitValue(allocator, val);
            }
        } else {
            // === Standard StdJson adapter path ===
            // Get env and build combined JSON if needed
            const env_val = obj.get("env");
            var combined_json: ?[]const u8 = null;
            defer if (combined_json) |cj| allocator.free(cj);

            const doc_json = if (env_val) |ev| blk: {
                const env_str = std.json.Stringify.valueAlloc(allocator, ev, .{}) catch "{}";
                defer allocator.free(env_str);
                combined_json = std.fmt.allocPrint(allocator, "{{\"__input\":{s},\"__env\":{s}}}", .{ input_str, env_str }) catch {
                    result.failed += 1;
                    result.parse_errors += 1;
                    try addFailure(allocator, failures, result.name, name_str, expr_str, "env json error", null, null);
                    continue;
                };
                break :blk combined_json.?;
            } else input_str;

            var eval_arena = std.heap.ArenaAllocator.init(allocator);
            defer eval_arena.deinit();
            const arena_alloc = eval_arena.allocator();

            const std_parsed = std.json.parseFromSliceLeaky(std.json.Value, arena_alloc, doc_json, .{ .parse_numbers = false }) catch {
                if (expect_error) {
                    result.passed += 1;
                    if (opts.verbose) {
                        std.debug.print("[PASS] {s}:{s} (expected json error)\n", .{ result.name, name_str });
                    }
                } else {
                    result.failed += 1;
                    result.parse_errors += 1;
                    try addFailure(allocator, failures, result.name, name_str, expr_str, "json parse error", null, null);
                }
                continue;
            };

            var adapter = StdJsonAdapter.init(arena_alloc);

            // Extract root and build env map if we have env values
            const root_ref: StdJsonAdapter.NodeRef = if (env_val != null)
                adapter.objectGet(&std_parsed, "__input") orelse &std_parsed
            else
                &std_parsed;

            var env_map = eval.Env.init(arena_alloc);
            var env_ptr: ?*eval.Env = null;

            if (env_val != null) {
                if (adapter.objectGet(&std_parsed, "__env")) |env_ref| {
                    if (adapter.kind(env_ref) == .object) {
                        var iter = adapter.objectIter(env_ref);
                        while (iter.next()) |entry| {
                            const env_item = makeEnvItem(&adapter, entry.value);
                            const slice = try arena_alloc.alloc(item.Item, 1);
                            slice[0] = env_item;
                            try env_map.put(entry.key, slice);
                        }
                        env_ptr = &env_map;
                    }
                }
            }

            var ctx = eval.EvalContext(StdJsonAdapter){
                .allocator = arena_alloc,
                .adapter = &adapter,
                .types = &types,
                .schema = schema_ptr,
                .timestamp = std.time.timestamp(),
            };
            var eval_result = eval.evalExpression(&ctx, expr, root_ref, env_ptr) catch {
                if (expect_error) {
                    result.passed += 1;
                    if (opts.verbose) {
                        std.debug.print("[PASS] {s}:{s} (expected eval error)\n", .{ result.name, name_str });
                    }
                } else {
                    result.failed += 1;
                    result.eval_errors += 1;
                    try addFailure(allocator, failures, result.name, name_str, expr_str, "eval error", null, null);
                }
                continue;
            };

            if (expect_error) {
                result.failed += 1;
                try addFailure(allocator, failures, result.name, name_str, expr_str, "expected error but succeeded", null, null);
                eval_result.deinit(arena_alloc);
                continue;
            }
            defer eval_result.deinit(arena_alloc);

            const expect_val = obj.get(expect_key);
            const empty_items: []const std.json.Value = &[_]std.json.Value{};
            var expect_items = empty_items;
            if (expect_val) |ev| {
                if (ev == .array) {
                    expect_items = ev.array.items;
                }
            }

            var actual_values = try stdItemsToJsonArray(allocator, &adapter, eval_result.items, &types, schema_ptr);
            defer actual_values.deinit(allocator);

            const unordered = if (obj.get("unordered")) |uv| uv == .bool and uv.bool else false;
            const matched = compareExpected(expect_items, actual_values.items, unordered);
            if (!matched) {
                result.failed += 1;
                result.mismatch_errors += 1;

                const expected_str = if (expect_val) |ev|
                    jsonStringifyOwned(allocator, ev)
                else
                    dupOwned(allocator, "[]");
                defer if (expected_str.owned) allocator.free(expected_str.buf);

                const actual_array = std.json.Array{ .items = actual_values.items, .capacity = actual_values.capacity, .allocator = allocator };
                const actual_str = jsonStringifyOwned(allocator, std.json.Value{ .array = actual_array });
                defer if (actual_str.owned) allocator.free(actual_str.buf);

                try addFailure(allocator, failures, result.name, name_str, expr_str, "mismatch", expected_str.buf, actual_str.buf);
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
    }

    return result;
}

fn jsonStringifyOwned(allocator: std.mem.Allocator, value: std.json.Value) OwnedStr {
    const buf = std.json.Stringify.valueAlloc(allocator, value, .{}) catch return .{
        .buf = "?",
        .owned = false,
    };
    return .{ .buf = buf, .owned = true };
}

fn dupOwned(allocator: std.mem.Allocator, text: []const u8) OwnedStr {
    const buf = allocator.dupe(u8, text) catch return .{
        .buf = "?",
        .owned = false,
    };
    return .{ .buf = buf, .owned = true };
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

fn stdItemsToJsonArray(
    allocator: std.mem.Allocator,
    adapter: *StdJsonAdapter,
    items: []const item.Item,
    types: *item.TypeTable,
    schema_ptr: ?*schema.Schema,
) !std.ArrayList(std.json.Value) {
    var out = std.ArrayList(std.json.Value).empty;
    for (items) |it| {
        var type_name: []const u8 = "";
        if (schema.isModelType(it.type_id)) {
            if (schema_ptr) |s| {
                type_name = s.typeName(it.type_id);
            }
        } else {
            type_name = types.name(it.type_id);
        }
        const val = try convert.adapterItemToTypedJsonValue(StdJsonAdapter, allocator, adapter, it, type_name);
        try out.append(allocator, val);
    }
    return out;
}

fn fhirItemsToJsonArray(
    allocator: std.mem.Allocator,
    adapter: *FhirJsonAdapter,
    items: []const item.Item,
    types: *item.TypeTable,
    schema_ptr: ?*schema.Schema,
) !std.ArrayList(std.json.Value) {
    var out = std.ArrayList(std.json.Value).empty;
    for (items) |it| {
        var type_name: []const u8 = "";
        if (schema_ptr) |s| {
            type_name = s.outputTypeName(it.type_id);
        } else if (!schema.isModelType(it.type_id)) {
            type_name = types.name(it.type_id);
        }
        const val = try convert.adapterItemToTypedJsonValue(FhirJsonAdapter, allocator, adapter, it, type_name);
        try out.append(allocator, val);
    }
    return out;
}

fn compareExpected(expected: []const std.json.Value, actual: []const std.json.Value, unordered: bool) bool {
    if (expected.len != actual.len) return false;
    
    if (unordered) {
        // Multiset comparison: each expected item must match exactly one actual item
        var used = [_]bool{false} ** 256; // Max 256 items
        if (actual.len > 256) return false; // Sanity check
        
        for (expected) |exp_item| {
            var found = false;
            for (actual, 0..) |act_item, j| {
                if (!used[j] and compareTypedValue(exp_item, act_item)) {
                    used[j] = true;
                    found = true;
                    break;
                }
            }
            if (!found) return false;
        }
        return true;
    }
    
    // Ordered comparison
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
        // Handle number coercion (integer vs float)
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
        .number_string => |v| {
            // Compare as numbers if both parseable, handles trailing zeros
            const na = std.fmt.parseFloat(f64, v) catch null;
            const nb = std.fmt.parseFloat(f64, b.number_string) catch null;
            if (na != null and nb != null) {
                return na.? == nb.?;
            }
            return std.mem.eql(u8, v, b.number_string);
        },
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
        .number_string, .string => |s| std.fmt.parseFloat(f64, s) catch null,
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

fn makeEnvItem(_: *StdJsonAdapter, ref: StdJsonAdapter.NodeRef) item.Item {
    return .{
        .data_kind = .node_ref,
        .value_kind = .empty,
        .type_id = 0,
        .source_pos = 0,
        .source_end = 0,
        .node = @intFromPtr(ref),
        .value = null,
    };
}
