//! End-to-end FHIRPath evaluation benchmarks
//! Measures: expression parsing + JSON parsing + evaluation
const std = @import("std");
const lib = @import("lib.zig");
const eval = @import("eval.zig");
const ast = @import("ast.zig");
const item = @import("item.zig");
const StdJsonAdapter = @import("backends/stdjson.zig").StdJsonAdapter;

var sink: usize = 0;

const PATIENT_JSON =
    \\{
    \\  "resourceType": "Patient",
    \\  "id": "example",
    \\  "identifier": [
    \\    {"system": "urn:oid:1.2.36.146.595.217.0.1", "value": "12345"},
    \\    {"system": "http://example.org/mrn", "value": "67890"},
    \\    {"system": "urn:foo", "value": "foo-value"}
    \\  ],
    \\  "name": [
    \\    {"use": "official", "family": "Chalmers", "given": ["Peter", "James"]},
    \\    {"use": "usual", "family": "Windsor", "given": ["Jim"]}
    \\  ],
    \\  "gender": "male",
    \\  "birthDate": "1974-12-25",
    \\  "active": true,
    \\  "address": [
    \\    {"line": ["123 Main St"], "city": "Boston", "state": "MA", "postalCode": "02101"}
    \\  ]
    \\}
;

const OBSERVATION_JSON =
    \\{
    \\  "resourceType": "Observation",
    \\  "id": "blood-pressure",
    \\  "status": "final",
    \\  "code": {
    \\    "coding": [{"system": "http://loinc.org", "code": "85354-9", "display": "Blood pressure"}]
    \\  },
    \\  "subject": {"reference": "Patient/example"},
    \\  "effectiveDateTime": "2023-01-15T10:30:00Z",
    \\  "component": [
    \\    {
    \\      "code": {"coding": [{"system": "http://loinc.org", "code": "8480-6", "display": "Systolic"}]},
    \\      "valueQuantity": {"value": 120, "unit": "mmHg", "system": "http://unitsofmeasure.org", "code": "mm[Hg]"}
    \\    },
    \\    {
    \\      "code": {"coding": [{"system": "http://loinc.org", "code": "8462-4", "display": "Diastolic"}]},
    \\      "valueQuantity": {"value": 80, "unit": "mmHg", "system": "http://unitsofmeasure.org", "code": "mm[Hg]"}
    \\    }
    \\  ]
    \\}
;

fn generateBundle(allocator: std.mem.Allocator, n: usize) ![]const u8 {
    var buf = std.ArrayList(u8).empty;
    const w = buf.writer(allocator);
    try w.writeAll(
        \\{"resourceType":"Bundle","type":"searchset","entry":[
    );
    for (0..n) |i| {
        if (i > 0) try w.writeByte(',');
        if (i % 3 == 0) {
            try w.print(
                \\{{"resource":{{"resourceType":"Patient","id":"p{d}","name":[{{"given":["Name{d}"],"family":"Family{d}"}}],"active":true}}}}
            , .{ i, i, i });
        } else if (i % 3 == 1) {
            try w.print(
                \\{{"resource":{{"resourceType":"Observation","id":"o{d}","status":"final","code":{{"coding":[{{"code":"code{d}"}}]}},"valueQuantity":{{"value":{d},"unit":"mg"}}}}}}
            , .{ i, i, i });
        } else {
            try w.print(
                \\{{"resource":{{"resourceType":"DiagnosticReport","id":"d{d}","status":"final","result":[{{"reference":"Observation/o{d}"}}]}}}}
            , .{ i, i });
        }
    }
    try w.writeAll("]}");
    return buf.toOwnedSlice(allocator);
}

const Scenario = struct {
    name: []const u8,
    expr: []const u8,
    json: []const u8,
};

fn runScenario(allocator: std.mem.Allocator, scenario: Scenario, iterations: usize, warmup: usize) !f64 {
    // Pre-parse expression (reused across iterations)
    const expr = try lib.parseExpression(allocator, scenario.expr);
    defer ast.deinitExpr(allocator, expr);

    // Warmup
    for (0..warmup) |_| {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const aa = arena.allocator();
        const parsed = try std.json.parseFromSliceLeaky(std.json.Value, aa, scenario.json, .{ .parse_numbers = false });
        var adapter = StdJsonAdapter.init(aa);
        var ctx = eval.EvalContext(StdJsonAdapter){
            .allocator = aa,
            .adapter = &adapter,
            .types = undefined, // Will init below
            .schema = null,
            .timestamp = 1706500000,
        };
        var types = try item.TypeTable.init(aa);
        ctx.types = &types;
        if (eval.evalExpression(&ctx, expr, &parsed, null)) |result| {
            sink +%= result.items.len;
        } else |_| {}
    }

    // Benchmark
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const aa = arena.allocator();
        const parsed = try std.json.parseFromSliceLeaky(std.json.Value, aa, scenario.json, .{ .parse_numbers = false });
        var adapter = StdJsonAdapter.init(aa);
        var types = try item.TypeTable.init(aa);
        var ctx = eval.EvalContext(StdJsonAdapter){
            .allocator = aa,
            .adapter = &adapter,
            .types = &types,
            .schema = null,
            .timestamp = 1706500000,
        };
        if (eval.evalExpression(&ctx, expr, &parsed, null)) |result| {
            sink +%= result.items.len;
        } else |_| {}
    }
    const elapsed = @as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start));
    return @as(f64, @floatFromInt(elapsed)) / @as(f64, @floatFromInt(iterations)) / 1000.0; // microseconds
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var iterations: usize = 10_000;
    var warmup: usize = 500;
    for (args[1..], 0..) |arg, i| {
        if (std.mem.eql(u8, arg, "--iterations") and i + 2 < args.len) {
            iterations = try std.fmt.parseInt(usize, args[i + 2], 10);
        } else if (std.mem.eql(u8, arg, "--warmup") and i + 2 < args.len) {
            warmup = try std.fmt.parseInt(usize, args[i + 2], 10);
        }
    }

    const bundle_json = try generateBundle(allocator, 50);
    defer allocator.free(bundle_json);

    const large_bundle_json = try generateBundle(allocator, 200);
    defer allocator.free(large_bundle_json);

    std.debug.print("\n", .{});
    std.debug.print("=" ** 75 ++ "\n", .{});
    std.debug.print("FHIRPath End-to-End Evaluation Benchmark\n", .{});
    std.debug.print("=" ** 75 ++ "\n", .{});
    std.debug.print("Iterations: {d}, Warmup: {d}\n", .{ iterations, warmup });
    std.debug.print("Each iteration = JSON parse + FHIRPath eval (expression pre-parsed)\n\n", .{});

    const scenarios = [_]Scenario{
        // Simple property access
        .{ .name = "resourceType (shallow)", .expr = "resourceType", .json = PATIENT_JSON },
        .{ .name = "name.given (2-level)", .expr = "name.given", .json = PATIENT_JSON },
        .{ .name = "name.where(use='official') (filter)", .expr = "name.where(use='official')", .json = PATIENT_JSON },
        .{ .name = "name.given.count() (aggregate)", .expr = "name.given.count()", .json = PATIENT_JSON },
        .{ .name = "name.exists() (existence)", .expr = "name.exists()", .json = PATIENT_JSON },
        .{ .name = "name.first().family (chain)", .expr = "name.first().family", .json = PATIENT_JSON },

        // String operations
        .{ .name = "name.first().family.lower() (string)", .expr = "name.first().family.lower()", .json = PATIENT_JSON },
        .{ .name = "id.startsWith('ex') (string)", .expr = "id.startsWith('ex')", .json = PATIENT_JSON },

        // Comparison / boolean
        .{ .name = "active = true (equality)", .expr = "active = true", .json = PATIENT_JSON },
        .{ .name = "gender = 'male' (string eq)", .expr = "gender = 'male'", .json = PATIENT_JSON },

        // Observation paths
        .{ .name = "code.coding.code (nested)", .expr = "code.coding.code", .json = OBSERVATION_JSON },
        .{ .name = "component.code.coding (deep)", .expr = "component.code.coding", .json = OBSERVATION_JSON },

        // Type conversions
        .{ .name = "1 + 2 (literal arithmetic)", .expr = "1 + 2", .json = "{}" },
        .{ .name = "'hello'.length() (string fn)", .expr = "'hello'.length()", .json = "{}" },

        // Bundle traversals
        .{ .name = "entry.resource (bundle 50)", .expr = "entry.resource", .json = bundle_json },
        .{ .name = "entry.resource.where(resourceType='Patient') (bundle 50)", .expr = "entry.resource.where(resourceType='Patient')", .json = bundle_json },
        .{ .name = "entry.resource.count() (bundle 50)", .expr = "entry.resource.count()", .json = bundle_json },
    };

    std.debug.print("{s:<55} {s:>10} {s:>12}\n", .{ "Scenario", "μs/eval", "evals/sec" });
    std.debug.print("-" ** 75 ++ "\n", .{});

    for (scenarios[0..]) |scenario| {
        const us = try runScenario(allocator, scenario, iterations, warmup);
        const ops = 1_000_000.0 / us;
        std.debug.print("{s:<55} {d:>8.1}μs  {d:>10.0}/s\n", .{ scenario.name, us, ops });
    }

    // Large bundle with fewer iterations
    {
        const bundle_iters = iterations / 10;
        const us = try runScenario(allocator, .{
            .name = "entry.resource.where(resourceType='Observation') (bundle 200)",
            .expr = "entry.resource.where(resourceType='Observation')",
            .json = large_bundle_json,
        }, bundle_iters, warmup / 10);
        const ops = 1_000_000.0 / us;
        std.debug.print("{s:<55} {d:>8.1}μs  {d:>10.0}/s\n", .{ "entry.resource.where(rType=Obs) (bundle 200)", us, ops });
    }

    // --- Parse-only vs eval-only breakdown ---
    std.debug.print("\n--- Parse vs Eval breakdown (Patient JSON) ---\n", .{});
    {
        // Parse-only
        const parse_start = std.time.nanoTimestamp();
        for (0..iterations) |_| {
            var arena = std.heap.ArenaAllocator.init(allocator);
            defer arena.deinit();
            _ = try std.json.parseFromSliceLeaky(std.json.Value, arena.allocator(), PATIENT_JSON, .{ .parse_numbers = false });
        }
        const parse_elapsed = @as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(parse_start));
        const parse_us = @as(f64, @floatFromInt(parse_elapsed)) / @as(f64, @floatFromInt(iterations)) / 1000.0;

        // Eval-only (pre-parsed)
        var pre_parsed = try std.json.parseFromSlice(std.json.Value, allocator, PATIENT_JSON, .{ .parse_numbers = false });
        defer pre_parsed.deinit();
        const expr_simple = try lib.parseExpression(allocator, "name.given");
        defer ast.deinitExpr(allocator, expr_simple);

        const eval_start = std.time.nanoTimestamp();
        for (0..iterations) |_| {
            var arena = std.heap.ArenaAllocator.init(allocator);
            defer arena.deinit();
            const aa = arena.allocator();
            var adapter = StdJsonAdapter.init(aa);
            var types = try item.TypeTable.init(aa);
            var ctx = eval.EvalContext(StdJsonAdapter){
                .allocator = aa,
                .adapter = &adapter,
                .types = &types,
                .schema = null,
                .timestamp = 1706500000,
            };
            if (eval.evalExpression(&ctx, expr_simple, &pre_parsed.value, null)) |result| {
                sink +%= result.items.len;
            } else |_| {}
        }
        const eval_elapsed = @as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(eval_start));
        const eval_us = @as(f64, @floatFromInt(eval_elapsed)) / @as(f64, @floatFromInt(iterations)) / 1000.0;

        std.debug.print("  JSON parse only:    {d:>8.1}μs\n", .{parse_us});
        std.debug.print("  Eval only:          {d:>8.1}μs\n", .{eval_us});
        std.debug.print("  Combined (above):   {d:>8.1}μs\n", .{parse_us + eval_us});
    }

    // --- Eval-only with shared TypeTable ---
    std.debug.print("\n--- Eval-only with reusable TypeTable (Patient JSON, name.given) ---\n", .{});
    {
        var pre_parsed2 = try std.json.parseFromSlice(std.json.Value, allocator, PATIENT_JSON, .{ .parse_numbers = false });
        defer pre_parsed2.deinit();
        const expr2 = try lib.parseExpression(allocator, "name.given");
        defer ast.deinitExpr(allocator, expr2);
        var shared_types = try item.TypeTable.init(allocator);
        defer shared_types.deinit();

        const eval_start2 = std.time.nanoTimestamp();
        for (0..iterations) |_| {
            var arena = std.heap.ArenaAllocator.init(allocator);
            defer arena.deinit();
            const aa = arena.allocator();
            var adapter = StdJsonAdapter.init(aa);
            var ctx = eval.EvalContext(StdJsonAdapter){
                .allocator = aa,
                .adapter = &adapter,
                .types = &shared_types,
                .schema = null,
                .timestamp = 1706500000,
            };
            if (eval.evalExpression(&ctx, expr2, &pre_parsed2.value, null)) |result| {
                sink +%= result.items.len;
            } else |_| {}
        }
        const eval_elapsed2 = @as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(eval_start2));
        const eval_us2 = @as(f64, @floatFromInt(eval_elapsed2)) / @as(f64, @floatFromInt(iterations)) / 1000.0;
        std.debug.print("  Eval with shared TypeTable: {d:>8.1}μs\n", .{eval_us2});
    }

    std.debug.print("\nSink: {d}\n", .{sink});
}
