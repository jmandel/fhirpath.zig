const std = @import("std");
const lib = @import("lib.zig");
const eval = @import("eval.zig");
const ast = @import("ast.zig");
const jsondoc = @import("jsondoc.zig");
const item = @import("item.zig");
const schema = @import("schema.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var tests_path: []const u8 = "tests/r5/tests-fhir-r5.json";
    var input_dir: []const u8 = "tests/r5/input";
    var model_path: ?[]const u8 = "models/r5/model.bin";
    var schema_prefix: []const u8 = "FHIR";
    var mode_filter: ?[]const u8 = null;
    var limit: usize = 0;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--tests") and i + 1 < args.len) {
            tests_path = args[i + 1];
            i += 1;
        } else if (std.mem.eql(u8, arg, "--input-dir") and i + 1 < args.len) {
            input_dir = args[i + 1];
            i += 1;
        } else if (std.mem.eql(u8, arg, "--model") and i + 1 < args.len) {
            model_path = args[i + 1];
            i += 1;
        } else if (std.mem.eql(u8, arg, "--prefix") and i + 1 < args.len) {
            schema_prefix = args[i + 1];
            i += 1;
        } else if (std.mem.eql(u8, arg, "--no-model")) {
            model_path = null;
        } else if (std.mem.eql(u8, arg, "--mode") and i + 1 < args.len) {
            mode_filter = args[i + 1];
            i += 1;
        } else if (std.mem.eql(u8, arg, "--limit") and i + 1 < args.len) {
            limit = std.fmt.parseInt(usize, args[i + 1], 10) catch 0;
            i += 1;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        }
    }

    const tests_json = try std.fs.cwd().readFileAlloc(allocator, tests_path, 50 * 1024 * 1024);
    defer allocator.free(tests_json);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, tests_json, .{});
    defer parsed.deinit();

    const root = parsed.value;
    const tests_val = root.object.get("tests") orelse {
        std.debug.print("Missing tests array\n", .{});
        return error.InvalidFormat;
    };
    if (tests_val != .array) return error.InvalidFormat;

    var types = try item.TypeTable.init(allocator);
    defer types.deinit();

    var schema_obj: ?schema.Schema = null;
    var model_bytes: ?[]u8 = null;
    if (model_path) |path| {
        if (std.fs.cwd().readFileAlloc(allocator, path, 128 * 1024 * 1024)) |bytes| {
            model_bytes = bytes;
        } else |err| {
            std.debug.print("Schema model load failed: {s} ({})\n", .{ path, err });
        }
        if (model_bytes) |bytes| {
            if (schema.Schema.init(allocator, "default", schema_prefix, bytes)) |schema_val| {
                schema_obj = schema_val;
            } else |err| {
                std.debug.print("Schema init failed: {}\n", .{err});
            }
        }
    }
    defer if (model_bytes) |bytes| allocator.free(bytes);
    defer if (schema_obj) |*s| s.deinit();

    var total: usize = 0;
    var passed: usize = 0;
    var failed: usize = 0;
    var skipped: usize = 0;
    var invalid_expected: usize = 0;

    for (tests_val.array.items) |test_val| {
        if (limit != 0 and total >= limit) break;
        if (test_val != .object) continue;
        const obj = test_val.object;
        const name_val = obj.get("name") orelse std.json.Value{ .string = "(unnamed)" };
        const expr_val = obj.get("expression") orelse continue;
        const inputfile_val = obj.get("inputfile") orelse std.json.Value{ .string = "" };
        const mode_val = obj.get("mode");
        const invalid = obj.get("invalid") != null;
        const outputs_val = obj.get("outputs");

        if (mode_filter) |mf| {
            if (mode_val == null) continue;
            if (mode_val.? != .string) continue;
            if (!std.mem.eql(u8, mode_val.?.string, mf)) continue;
        }

        total += 1;

        if (expr_val != .string) {
            failCase(name_val, "invalid expression type");
            failed += 1;
            continue;
        }

        const expr = lib.parseExpression(allocator, expr_val.string) catch {
            if (invalid) {
                passed += 1;
                invalid_expected += 1;
            } else {
                failCase(name_val, "parse error");
                failed += 1;
            }
            continue;
        };
        defer ast.deinitExpr(allocator, expr);

        const input_json = loadInputJson(allocator, input_dir, inputfile_val) catch |err| {
            if (err == error.InputMissing) {
                skipped += 1;
                continue;
            }
            if (invalid) {
                passed += 1;
                invalid_expected += 1;
            } else {
                failCase(name_val, "input read error");
                failed += 1;
            }
            continue;
        };
        defer allocator.free(input_json);

        var doc = jsondoc.JsonDoc.init(allocator, input_json) catch {
            if (invalid) {
                passed += 1;
                invalid_expected += 1;
            } else {
                failCase(name_val, "json parse error");
                failed += 1;
            }
            continue;
        };
        defer doc.deinit();

        var ctx = eval.EvalContext{
            .allocator = allocator,
            .doc = &doc,
            .types = &types,
            .schema = if (schema_obj) |*s| s else null,
        };
        var items = eval.evalExpression(&ctx, expr, doc.root, null) catch {
            if (invalid) {
                passed += 1;
                invalid_expected += 1;
            } else {
                failCase(name_val, "eval error");
                failed += 1;
            }
            continue;
        };
        defer items.deinit(allocator);

        if (invalid) {
            failCase(name_val, "expected invalid, but succeeded");
            failed += 1;
            continue;
        }

        const expected = outputs_val orelse std.json.Value{ .array = std.json.Array{ .items = &[_]std.json.Value{}, .capacity = 0, .allocator = allocator } };
        if (expected != .array) {
            failCase(name_val, "outputs not array");
            failed += 1;
            continue;
        }

        if (!compareOutputs(&types, if (schema_obj) |*s| s else null, &doc, expected.array.items, items.items)) {
            failCase(name_val, "mismatch");
            failed += 1;
            continue;
        }

        passed += 1;
    }

    std.debug.print("Official tests: total={d} passed={d} failed={d} skipped={d} invalid_expected={d}\n", .{ total, passed, failed, skipped, invalid_expected });
    if (failed > 0) return error.TestFailed;
}

fn loadInputJson(allocator: std.mem.Allocator, input_dir: []const u8, inputfile_val: std.json.Value) ![]const u8 {
    if (inputfile_val != .string or inputfile_val.string.len == 0) {
        return allocator.dupe(u8, "{}");
    }
    const inputfile = inputfile_val.string;
    const cwd = std.fs.cwd();
    var path_buf = std.ArrayListUnmanaged(u8){};
    defer path_buf.deinit(allocator);
    try path_buf.appendSlice(allocator, input_dir);
    if (path_buf.items.len > 0 and path_buf.items[path_buf.items.len - 1] != std.fs.path.sep) {
        try path_buf.append(allocator, std.fs.path.sep);
    }
    try path_buf.appendSlice(allocator, inputfile);
    const full_path = path_buf.items;
    if (cwd.readFileAlloc(allocator, full_path, 10 * 1024 * 1024)) |data| {
        return data;
    } else |_| {}
    // try .xml -> .json
    if (std.mem.endsWith(u8, inputfile, ".xml")) {
        path_buf.clearRetainingCapacity();
        try path_buf.appendSlice(allocator, input_dir);
        if (path_buf.items.len > 0 and path_buf.items[path_buf.items.len - 1] != std.fs.path.sep) {
            try path_buf.append(allocator, std.fs.path.sep);
        }
        try path_buf.appendSlice(allocator, inputfile[0 .. inputfile.len - 4]);
        try path_buf.appendSlice(allocator, ".json");
        if (cwd.readFileAlloc(allocator, path_buf.items, 10 * 1024 * 1024)) |data| {
            return data;
        } else |_| {}
    }
    return error.InputMissing;
}

fn compareOutputs(
    types: *item.TypeTable,
    schema_opt: ?*schema.Schema,
    doc: *jsondoc.JsonDoc,
    expected: []const std.json.Value,
    actual: []const item.Item,
) bool {
    if (expected.len != actual.len) return false;
    var i: usize = 0;
    while (i < expected.len) : (i += 1) {
        if (expected[i] != .object) return false;
        const obj = expected[i].object;
        const type_val = obj.get("type") orelse return false;
        const value_val = obj.get("value") orelse return false;
        if (type_val != .string or value_val != .string) return false;
        if (!typeMatches(types, schema_opt, actual[i], type_val.string)) return false;
        if (!valueMatches(doc, actual[i], type_val.string, value_val.string)) return false;
    }
    return true;
}

fn typeMatches(types: *item.TypeTable, schema_opt: ?*schema.Schema, it: item.Item, expected: []const u8) bool {
    const actual_name = typeName(types, schema_opt, it.type_id);
    if (actual_name.len == 0) return false;
    if (std.mem.indexOfScalar(u8, expected, '.') != null) {
        return eqlIgnoreCase(actual_name, expected);
    }
    const final = lastSegment(actual_name);
    return eqlIgnoreCase(final, expected);
}

fn typeName(types: *item.TypeTable, schema_opt: ?*schema.Schema, type_id: u32) []const u8 {
    if (schema.isModelType(type_id)) {
        if (schema_opt) |s| return s.typeName(type_id);
        return "";
    }
    return types.name(type_id);
}

fn valueMatches(doc: *jsondoc.JsonDoc, it: item.Item, expected_type: []const u8, expected_value: []const u8) bool {
    if (eqlIgnoreCase(expected_type, "boolean")) {
        const want = eqlIgnoreCase(expected_value, "true");
        const got = itemBool(doc, it) orelse return false;
        return got == want;
    }
    if (eqlIgnoreCase(expected_type, "integer")) {
        const want = std.fmt.parseInt(i64, expected_value, 10) catch return false;
        const got = itemInteger(doc, it) orelse return false;
        return got == want;
    }
    if (eqlIgnoreCase(expected_type, "decimal")) {
        const want = std.fmt.parseFloat(f64, expected_value) catch return false;
        const got = itemDecimal(doc, it) orelse return false;
        return got == want;
    }
    if (eqlIgnoreCase(expected_type, "quantity")) {
        // Expect format: "<num> '<unit>'"
        const q = parseQuantity(expected_value) orelse return false;
        const got = itemQuantity(doc, it) orelse return false;
        return std.mem.eql(u8, q.value, got.value) and std.mem.eql(u8, q.unit, got.unit);
    }
    // string-like types
    const got_str = itemString(doc, it) orelse return false;
    return std.mem.eql(u8, got_str, expected_value);
}

fn itemString(doc: *jsondoc.JsonDoc, it: item.Item) ?[]const u8 {
    if (it.data_kind == .value and it.value != null) {
        return switch (it.value.?) {
            .string => |s| s,
            .date => |s| s,
            .time => |s| s,
            .dateTime => |s| s,
            .decimal => |s| s,
            else => null,
        };
    }
    if (it.data_kind == .json_span and it.node != null) {
        const node = doc.node(@intCast(it.node.?)).*;
        if (node.kind == .string) return node.data.string;
        if (node.kind == .number) return node.data.number;
    }
    return null;
}

fn itemBool(doc: *jsondoc.JsonDoc, it: item.Item) ?bool {
    if (it.data_kind == .value and it.value != null and it.value.? == .boolean) return it.value.?.boolean;
    if (it.data_kind == .json_span and it.node != null) {
        const node = doc.node(@intCast(it.node.?)).*;
        if (node.kind == .bool) return node.data.bool;
    }
    return null;
}

fn itemInteger(doc: *jsondoc.JsonDoc, it: item.Item) ?i64 {
    if (it.data_kind == .value and it.value != null and it.value.? == .integer) return it.value.?.integer;
    if (it.data_kind == .json_span and it.node != null) {
        const node = doc.node(@intCast(it.node.?)).*;
        if (node.kind == .number) return std.fmt.parseInt(i64, node.data.number, 10) catch null;
    }
    return null;
}

fn itemDecimal(doc: *jsondoc.JsonDoc, it: item.Item) ?f64 {
    if (it.data_kind == .value and it.value != null) {
        return switch (it.value.?) {
            .decimal => |s| std.fmt.parseFloat(f64, s) catch null,
            .integer => |v| @floatFromInt(v),
            else => null,
        };
    }
    if (it.data_kind == .json_span and it.node != null) {
        const node = doc.node(@intCast(it.node.?)).*;
        if (node.kind == .number) return std.fmt.parseFloat(f64, node.data.number) catch null;
    }
    return null;
}

fn itemQuantity(doc: *jsondoc.JsonDoc, it: item.Item) ?item.Quantity {
    _ = doc;
    if (it.data_kind == .value and it.value != null and it.value.? == .quantity) return it.value.?.quantity;
    return null;
}

fn parseQuantity(text: []const u8) ?item.Quantity {
    const first_space = std.mem.indexOfScalar(u8, text, ' ') orelse return null;
    const value = text[0..first_space];
    const rest = text[first_space + 1 ..];
    if (rest.len < 2) return null;
    if (rest[0] != '\'' or rest[rest.len - 1] != '\'') return null;
    const unit = rest[1 .. rest.len - 1];
    return .{ .value = value, .unit = unit };
}

fn lastSegment(name: []const u8) []const u8 {
    if (std.mem.lastIndexOfScalar(u8, name, '.')) |idx| {
        return name[idx + 1 ..];
    }
    return name;
}

fn eqlIgnoreCase(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        if (std.ascii.toLower(a[i]) != std.ascii.toLower(b[i])) return false;
    }
    return true;
}

fn failCase(name_val: std.json.Value, msg: []const u8) void {
    if (name_val == .string) {
        std.debug.print("[FAIL] {s}: {s}\n", .{ name_val.string, msg });
    } else {
        std.debug.print("[FAIL] {s}\n", .{ msg });
    }
}

fn printUsage() void {
    const usage =
        \\Usage: official-tests [--tests path] [--input-dir path] [--mode name] [--limit N]
        \\                      [--model path] [--prefix name] [--no-model]
        \\ 
        \\Runs the official FHIRPath JSON test suite.
        \\ 
    ;
    std.debug.print("{s}", .{usage});
}
