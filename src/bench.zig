//! Performance benchmarks for JsonAdapter navigation patterns
const std = @import("std");
const node = @import("node.zig");
const JsonAdapter = @import("backends/json_adapter.zig").JsonAdapter;

// Prevent the optimizer from eliminating benchmark code
var sink: usize = 0;
fn doNotOptimize(val: anytype) void {
    const T = @TypeOf(val);
    const info = @typeInfo(T);
    if (info == .pointer and info.pointer.size != .slice) {
        sink +%= @intFromPtr(val);
    } else if (info == .optional) {
        if (val) |v| doNotOptimize(v);
    } else if (T == []const u8) {
        sink +%= val.len;
        if (val.len > 0) sink +%= val[0];
    } else if (T == usize or T == u64 or T == i64) {
        sink +%= val;
    } else if (@sizeOf(T) > 0) {
        sink +%= @sizeOf(T);
    }
}

const PATIENT_JSON =
    \\{
    \\  "resourceType": "Patient",
    \\  "id": "example",
    \\  "meta": {"profile": ["http://hl7.org/fhir/StructureDefinition/Patient"]},
    \\  "identifier": [
    \\    {"system": "urn:oid:1.2.36.146.595.217.0.1", "value": "12345"},
    \\    {"system": "http://example.org/mrn", "value": "67890"},
    \\    {"system": "urn:foo", "value": "foo-value"}
    \\  ],
    \\  "name": [
    \\    {"family": "Chalmers", "given": ["Peter", "James"]},
    \\    {"family": "Windsor", "given": ["Jim"]}
    \\  ],
    \\  "telecom": [
    \\    {"system": "phone", "value": "555-1234"},
    \\    {"system": "email", "value": "test@example.org"},
    \\    {"system": "phone", "value": "555-5678"}
    \\  ],
    \\  "gender": "male",
    \\  "birthDate": "1974-12-25",
    \\  "address": [
    \\    {"line": ["123 Main St", "Apt 4"], "city": "Boston", "state": "MA"}
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
    \\  "performer": [{"reference": "Practitioner/example"}],
    \\  "component": [
    \\    {
    \\      "code": {"coding": [{"system": "http://loinc.org", "code": "8480-6"}]},
    \\      "valueQuantity": {"value": 120, "unit": "mmHg"}
    \\    },
    \\    {
    \\      "code": {"coding": [{"system": "http://loinc.org", "code": "8462-4"}]},
    \\      "valueQuantity": {"value": 80, "unit": "mmHg"}
    \\    }
    \\  ]
    \\}
;

fn generateBundle(allocator: std.mem.Allocator, num_entries: usize) ![]const u8 {
    var buf = std.ArrayList(u8).empty;
    const w = buf.writer(allocator);

    try w.writeAll(
        \\{"resourceType": "Bundle", "id": "bundle-1", "type": "searchset", "entry": [
    );

    for (0..num_entries) |i| {
        if (i > 0) try w.writeByte(',');
        if (i % 2 == 0) {
            try w.print(
                \\{{"resource": {{"resourceType": "Patient", "id": "p{d}", "name": [{{"given": ["Name{d}"]}}]}}}}
            , .{ i, i });
        } else {
            try w.print(
                \\{{"resource": {{"resourceType": "Observation", "id": "o{d}", "code": {{"coding": [{{"code": "code{d}"}}]}}, "valueQuantity": {{"value": {d}}}}}}}
            , .{ i, i, i });
        }
    }

    try w.writeAll("]}");
    return buf.toOwnedSlice(allocator);
}

// ============================================================================
// Benchmark Functions
// ============================================================================

fn runSimpleNested(adapter: *JsonAdapter, root: JsonAdapter.NodeRef) void {
    if (adapter.objectGet(root, "code")) |code_ref| {
        if (adapter.objectGet(code_ref, "coding")) |coding| {
            if (adapter.kind(coding) == .array and adapter.arrayLen(coding) > 0) {
                const first = adapter.arrayAt(coding, 0);
                if (adapter.objectGet(first, "code")) |c| {
                    doNotOptimize(adapter.string(c));
                }
            }
        }
    }
}

fn runArrayTraversal(adapter: *JsonAdapter, root: JsonAdapter.NodeRef) void {
    if (adapter.objectGet(root, "name")) |names| {
        if (adapter.kind(names) == .array) {
            for (0..adapter.arrayLen(names)) |i| {
                const name_ref = adapter.arrayAt(names, i);
                if (adapter.objectGet(name_ref, "given")) |given| {
                    if (adapter.kind(given) == .array) {
                        for (0..adapter.arrayLen(given)) |j| {
                            const g = adapter.arrayAt(given, j);
                            doNotOptimize(adapter.string(g));
                        }
                    }
                }
            }
        }
    }
}

fn runDeepComponent(adapter: *JsonAdapter, root: JsonAdapter.NodeRef) void {
    if (adapter.objectGet(root, "component")) |components| {
        if (adapter.kind(components) == .array) {
            for (0..adapter.arrayLen(components)) |i| {
                const comp = adapter.arrayAt(components, i);
                if (adapter.objectGet(comp, "code")) |code_ref| {
                    if (adapter.objectGet(code_ref, "coding")) |coding| {
                        if (adapter.kind(coding) == .array and adapter.arrayLen(coding) > 0) {
                            const first = adapter.arrayAt(coding, 0);
                            if (adapter.objectGet(first, "code")) |c| {
                                doNotOptimize(adapter.string(c));
                            }
                        }
                    }
                }
            }
        }
    }
}

fn runWhereFilter(adapter: *JsonAdapter, root: JsonAdapter.NodeRef) void {
    if (adapter.objectGet(root, "identifier")) |ids| {
        if (adapter.kind(ids) == .array) {
            for (0..adapter.arrayLen(ids)) |i| {
                const id = adapter.arrayAt(ids, i);
                if (adapter.objectGet(id, "system")) |sys| {
                    if (std.mem.eql(u8, adapter.string(sys), "urn:foo")) {
                        if (adapter.objectGet(id, "value")) |val| {
                            doNotOptimize(adapter.string(val));
                        }
                    }
                }
            }
        }
    }
}

fn runBundleTraversal(adapter: *JsonAdapter, root: JsonAdapter.NodeRef) void {
    if (adapter.objectGet(root, "entry")) |entries| {
        if (adapter.kind(entries) == .array) {
            for (0..adapter.arrayLen(entries)) |i| {
                const entry = adapter.arrayAt(entries, i);
                if (adapter.objectGet(entry, "resource")) |resource| {
                    if (adapter.objectGet(resource, "resourceType")) |rt| {
                        if (std.mem.eql(u8, adapter.string(rt), "Observation")) {
                            if (adapter.objectGet(resource, "code")) |code_ref| {
                                if (adapter.objectGet(code_ref, "coding")) |coding| {
                                    if (adapter.kind(coding) == .array and adapter.arrayLen(coding) > 0) {
                                        const first = adapter.arrayAt(coding, 0);
                                        if (adapter.objectGet(first, "code")) |c| {
                                            doNotOptimize(adapter.string(c));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn runCount(adapter: *JsonAdapter, root: JsonAdapter.NodeRef) void {
    var count: usize = 0;
    if (adapter.objectGet(root, "name")) |names| {
        if (adapter.kind(names) == .array) {
            for (0..adapter.arrayLen(names)) |i| {
                const name_ref = adapter.arrayAt(names, i);
                if (adapter.objectGet(name_ref, "given")) |given| {
                    if (adapter.kind(given) == .array) {
                        count += adapter.arrayLen(given);
                    }
                }
            }
        }
    }
    doNotOptimize(count);
}

fn benchNavOnly(comptime runFn: anytype, adapter: *JsonAdapter, root: JsonAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        runFn(adapter, root);
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchParseEach(comptime runFn: anytype, allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_text, .{ .parse_numbers = false });
        defer parsed.deinit();
        var adapter = JsonAdapter.init(allocator, &parsed.value, .generic_json);
        runFn(&adapter, adapter.root());
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn printResult(name: []const u8, ns: u64, iterations: usize) void {
    const ms = @as(f64, @floatFromInt(ns)) / 1_000_000.0;
    const ops = @as(f64, @floatFromInt(iterations)) / (@as(f64, @floatFromInt(ns)) / 1_000_000_000.0);
    std.debug.print("{s:<40} {d:>8.2}ms  ({d:.0} ops/s)\n", .{ name, ms, ops });
}

const BenchOptions = struct {
    parse_each: bool,
    iterations: usize,
    bundle_size: usize,
};

fn parseArgs(allocator: std.mem.Allocator) !BenchOptions {
    var opts = BenchOptions{
        .parse_each = false,
        .iterations = 50_000,
        .bundle_size = 100,
    };

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--parse-each")) {
            opts.parse_each = true;
        } else if (std.mem.eql(u8, arg, "--iterations") and i + 1 < args.len) {
            i += 1;
            opts.iterations = try std.fmt.parseInt(usize, args[i], 10);
        } else if (std.mem.eql(u8, arg, "--bundle-size") and i + 1 < args.len) {
            i += 1;
            opts.bundle_size = try std.fmt.parseInt(usize, args[i], 10);
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            std.debug.print(
                \\Usage: zig build bench -- [--parse-each] [--iterations N] [--bundle-size N]
                \\  --parse-each   Parse JSON inside the loop (parse+eval)
                \\  --iterations   Number of iterations (default: 50000)
                \\  --bundle-size  Bundle entry count (default: 100)
                \\
            , .{});
            std.process.exit(0);
        }
    }

    return opts;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const options = try parseArgs(allocator);
    const iterations = options.iterations;
    const bundle_size = options.bundle_size;
    const bundle_iters = if (iterations >= 10) iterations / 10 else 1;

    std.debug.print("\n", .{});
    std.debug.print("=" ** 70 ++ "\n", .{});
    std.debug.print("JsonAdapter Performance Benchmark\n", .{});
    std.debug.print("=" ** 70 ++ "\n\n", .{});

    const bundle_json = try generateBundle(allocator, bundle_size);
    defer allocator.free(bundle_json);

    std.debug.print("Test Data:\n", .{});
    std.debug.print("  Patient JSON:     {d:>6} bytes\n", .{PATIENT_JSON.len});
    std.debug.print("  Observation JSON: {d:>6} bytes\n", .{OBSERVATION_JSON.len});
    std.debug.print("  Bundle JSON:      {d:>6} bytes ({d} entries)\n", .{ bundle_json.len, bundle_size });
    std.debug.print("  Iterations:       {d:>6}\n\n", .{iterations});
    std.debug.print("  Mode:             {s}\n\n", .{if (options.parse_each) "parse-each (parse+nav)" else "parse-once (navigation only)"});

    std.debug.print("Benchmark                                    Time        Throughput\n", .{});
    std.debug.print("-" ** 70 ++ "\n", .{});

    if (options.parse_each) {
        printResult("code.coding.code (Observation)", try benchParseEach(runSimpleNested, allocator, OBSERVATION_JSON, iterations), iterations);
        printResult("name.given (Patient)", try benchParseEach(runArrayTraversal, allocator, PATIENT_JSON, iterations), iterations);
        printResult("component.code.coding.code (Obs)", try benchParseEach(runDeepComponent, allocator, OBSERVATION_JSON, iterations), iterations);
        printResult("identifier.where(sys=X).value (Pt)", try benchParseEach(runWhereFilter, allocator, PATIENT_JSON, iterations), iterations);
        printResult("name.given.count() (Patient)", try benchParseEach(runCount, allocator, PATIENT_JSON, iterations), iterations);
        printResult("Bundle filter+traverse (100 entries)", try benchParseEach(runBundleTraversal, allocator, bundle_json, bundle_iters), bundle_iters);
    } else {
        const patient_parsed = try std.json.parseFromSlice(std.json.Value, allocator, PATIENT_JSON, .{ .parse_numbers = false });
        defer patient_parsed.deinit();
        var patient_adapter = JsonAdapter.init(allocator, &patient_parsed.value, .generic_json);

        const obs_parsed = try std.json.parseFromSlice(std.json.Value, allocator, OBSERVATION_JSON, .{ .parse_numbers = false });
        defer obs_parsed.deinit();
        var obs_adapter = JsonAdapter.init(allocator, &obs_parsed.value, .generic_json);

        const bundle_parsed = try std.json.parseFromSlice(std.json.Value, allocator, bundle_json, .{ .parse_numbers = false });
        defer bundle_parsed.deinit();
        var bundle_adapter = JsonAdapter.init(allocator, &bundle_parsed.value, .generic_json);

        printResult("code.coding.code (Observation)", benchNavOnly(runSimpleNested, &obs_adapter, obs_adapter.root(), iterations), iterations);
        printResult("name.given (Patient)", benchNavOnly(runArrayTraversal, &patient_adapter, patient_adapter.root(), iterations), iterations);
        printResult("component.code.coding.code (Obs)", benchNavOnly(runDeepComponent, &obs_adapter, obs_adapter.root(), iterations), iterations);
        printResult("identifier.where(sys=X).value (Pt)", benchNavOnly(runWhereFilter, &patient_adapter, patient_adapter.root(), iterations), iterations);
        printResult("name.given.count() (Patient)", benchNavOnly(runCount, &patient_adapter, patient_adapter.root(), iterations), iterations);
        printResult("Bundle filter+traverse (100 entries)", benchNavOnly(runBundleTraversal, &bundle_adapter, bundle_adapter.root(), bundle_iters), bundle_iters);
    }

    std.debug.print("\nSink: {d} (prevents optimization)\n", .{sink});
}
