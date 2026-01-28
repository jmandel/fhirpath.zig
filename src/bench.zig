//! Performance benchmarks comparing NodeAdapter implementations
//! 
//! Tests realistic FHIRPath-like navigation patterns on FHIR resources.
const std = @import("std");
const jsondoc = @import("jsondoc.zig");
const node = @import("node.zig");
const JsonDocAdapter = @import("backends/jsondoc.zig").JsonDocAdapter;
const StdJsonAdapter = @import("backends/stdjson.zig").StdJsonAdapter;

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

// ============================================================================
// Sample FHIR Resources
// ============================================================================

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

// Generate a Bundle with multiple entries
fn generateBundle(allocator: std.mem.Allocator, num_entries: usize) ![]const u8 {
    var buf = std.ArrayList(u8).empty;
    const w = buf.writer(allocator);
    
    try w.writeAll(
        \\{"resourceType": "Bundle", "id": "bundle-1", "type": "searchset", "entry": [
    );
    
    for (0..num_entries) |i| {
        if (i > 0) try w.writeByte(',');
        // Alternate between Patient and Observation
        if (i % 2 == 0) {
            try w.print(
                \\{{"resource": {{"resourceType": "Patient", "id": "p{d}", "name": [{{"given": ["Name{d}"]}}]}}}}
            , .{i, i});
        } else {
            try w.print(
                \\{{"resource": {{"resourceType": "Observation", "id": "o{d}", "code": {{"coding": [{{"code": "code{d}"}}]}}, "valueQuantity": {{"value": {d}}}}}}}
            , .{i, i, i});
        }
    }
    
    try w.writeAll("]}");
    return buf.toOwnedSlice(allocator);
}

// ============================================================================
// Benchmark Functions - JsonDocAdapter
// ============================================================================

fn runJsonDoc_SimpleNested(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef) void {
    // code.coding.code (3-level nest)
    if (adapter.objectGet(root, "code")) |code| {
        if (adapter.objectGet(code, "coding")) |coding| {
            if (adapter.kind(coding) == .array and adapter.arrayLen(coding) > 0) {
                const first = adapter.arrayAt(coding, 0);
                if (adapter.objectGet(first, "code")) |c| {
                    doNotOptimize(adapter.string(c));
                }
            }
        }
    }
}

fn benchJsonDoc_SimpleNested(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runJsonDoc_SimpleNested(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn runJsonDoc_ArrayTraversal(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef) void {
    // name.given (collect all given names from all name entries)
    if (adapter.objectGet(root, "name")) |names| {
        if (adapter.kind(names) == .array) {
            for (0..adapter.arrayLen(names)) |i| {
                const name = adapter.arrayAt(names, i);
                if (adapter.objectGet(name, "given")) |given| {
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

fn benchJsonDoc_ArrayTraversal(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runJsonDoc_ArrayTraversal(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn runJsonDoc_DeepComponent(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef) void {
    // component.code.coding.code (4-level with arrays)
    if (adapter.objectGet(root, "component")) |components| {
        if (adapter.kind(components) == .array) {
            for (0..adapter.arrayLen(components)) |i| {
                const comp = adapter.arrayAt(components, i);
                if (adapter.objectGet(comp, "code")) |code| {
                    if (adapter.objectGet(code, "coding")) |coding| {
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

fn benchJsonDoc_DeepComponent(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runJsonDoc_DeepComponent(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn runJsonDoc_WhereFilter(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef) void {
    // identifier.where(system='urn:foo').value
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

fn benchJsonDoc_WhereFilter(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runJsonDoc_WhereFilter(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn runJsonDoc_BundleTraversal(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef) void {
    // entry.resource.where(resourceType='Observation').code.coding.code
    if (adapter.objectGet(root, "entry")) |entries| {
        if (adapter.kind(entries) == .array) {
            for (0..adapter.arrayLen(entries)) |i| {
                const entry = adapter.arrayAt(entries, i);
                if (adapter.objectGet(entry, "resource")) |resource| {
                    if (adapter.objectGet(resource, "resourceType")) |rt| {
                        if (std.mem.eql(u8, adapter.string(rt), "Observation")) {
                            if (adapter.objectGet(resource, "code")) |code| {
                                if (adapter.objectGet(code, "coding")) |coding| {
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

fn benchJsonDoc_BundleTraversal(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runJsonDoc_BundleTraversal(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn runJsonDoc_Count(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef) void {
    // name.given.count() - count all given names
    var count: usize = 0;
    if (adapter.objectGet(root, "name")) |names| {
        if (adapter.kind(names) == .array) {
            for (0..adapter.arrayLen(names)) |i| {
                const name = adapter.arrayAt(names, i);
                if (adapter.objectGet(name, "given")) |given| {
                    if (adapter.kind(given) == .array) {
                        count += adapter.arrayLen(given);
                    }
                }
            }
        }
    }
    doNotOptimize(count);
}

fn benchJsonDoc_Count(adapter: *JsonDocAdapter, root: JsonDocAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runJsonDoc_Count(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

// ============================================================================
// Benchmark Functions - StdJsonAdapter
// ============================================================================

fn runStdJson_SimpleNested(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef) void {
    if (adapter.objectGet(root, "code")) |code| {
        if (adapter.objectGet(code, "coding")) |coding| {
            if (adapter.kind(coding) == .array and adapter.arrayLen(coding) > 0) {
                const first = adapter.arrayAt(coding, 0);
                if (adapter.objectGet(first, "code")) |c| {
                    doNotOptimize(adapter.string(c));
                }
            }
        }
    }
}

fn benchStdJson_SimpleNested(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runStdJson_SimpleNested(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn runStdJson_ArrayTraversal(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef) void {
    if (adapter.objectGet(root, "name")) |names| {
        if (adapter.kind(names) == .array) {
            for (0..adapter.arrayLen(names)) |i| {
                const name = adapter.arrayAt(names, i);
                if (adapter.objectGet(name, "given")) |given| {
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

fn benchStdJson_ArrayTraversal(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runStdJson_ArrayTraversal(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn runStdJson_DeepComponent(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef) void {
    if (adapter.objectGet(root, "component")) |components| {
        if (adapter.kind(components) == .array) {
            for (0..adapter.arrayLen(components)) |i| {
                const comp = adapter.arrayAt(components, i);
                if (adapter.objectGet(comp, "code")) |code| {
                    if (adapter.objectGet(code, "coding")) |coding| {
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

fn benchStdJson_DeepComponent(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runStdJson_DeepComponent(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn runStdJson_WhereFilter(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef) void {
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

fn benchStdJson_WhereFilter(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runStdJson_WhereFilter(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn runStdJson_BundleTraversal(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef) void {
    if (adapter.objectGet(root, "entry")) |entries| {
        if (adapter.kind(entries) == .array) {
            for (0..adapter.arrayLen(entries)) |i| {
                const entry = adapter.arrayAt(entries, i);
                if (adapter.objectGet(entry, "resource")) |resource| {
                    if (adapter.objectGet(resource, "resourceType")) |rt| {
                        if (std.mem.eql(u8, adapter.string(rt), "Observation")) {
                            if (adapter.objectGet(resource, "code")) |code| {
                                if (adapter.objectGet(code, "coding")) |coding| {
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

fn benchStdJson_BundleTraversal(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runStdJson_BundleTraversal(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn runStdJson_Count(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef) void {
    var count: usize = 0;
    if (adapter.objectGet(root, "name")) |names| {
        if (adapter.kind(names) == .array) {
            for (0..adapter.arrayLen(names)) |i| {
                const name = adapter.arrayAt(names, i);
                if (adapter.objectGet(name, "given")) |given| {
                    if (adapter.kind(given) == .array) {
                        count += adapter.arrayLen(given);
                    }
                }
            }
        }
    }
    doNotOptimize(count);
}

fn benchStdJson_Count(adapter: *StdJsonAdapter, root: StdJsonAdapter.NodeRef, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        runStdJson_Count(adapter, root);
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

// ============================================================================
// Benchmark Functions - Parse Each (includes parse cost)
// ============================================================================

fn benchJsonDoc_SimpleNestedParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        var doc = try jsondoc.JsonDoc.init(allocator, json_text);
        defer doc.deinit();
        var adapter = JsonDocAdapter.init(&doc);
        runJsonDoc_SimpleNested(&adapter, adapter.root());
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchJsonDoc_ArrayTraversalParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        var doc = try jsondoc.JsonDoc.init(allocator, json_text);
        defer doc.deinit();
        var adapter = JsonDocAdapter.init(&doc);
        runJsonDoc_ArrayTraversal(&adapter, adapter.root());
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchJsonDoc_DeepComponentParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        var doc = try jsondoc.JsonDoc.init(allocator, json_text);
        defer doc.deinit();
        var adapter = JsonDocAdapter.init(&doc);
        runJsonDoc_DeepComponent(&adapter, adapter.root());
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchJsonDoc_WhereFilterParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        var doc = try jsondoc.JsonDoc.init(allocator, json_text);
        defer doc.deinit();
        var adapter = JsonDocAdapter.init(&doc);
        runJsonDoc_WhereFilter(&adapter, adapter.root());
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchJsonDoc_CountParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        var doc = try jsondoc.JsonDoc.init(allocator, json_text);
        defer doc.deinit();
        var adapter = JsonDocAdapter.init(&doc);
        runJsonDoc_Count(&adapter, adapter.root());
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchJsonDoc_BundleTraversalParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        var doc = try jsondoc.JsonDoc.init(allocator, json_text);
        defer doc.deinit();
        var adapter = JsonDocAdapter.init(&doc);
        runJsonDoc_BundleTraversal(&adapter, adapter.root());
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchStdJson_SimpleNestedParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_text, .{ .parse_numbers = false });
        defer parsed.deinit();
        var adapter = StdJsonAdapter.init(allocator);
        runStdJson_SimpleNested(&adapter, &parsed.value);
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchStdJson_ArrayTraversalParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_text, .{ .parse_numbers = false });
        defer parsed.deinit();
        var adapter = StdJsonAdapter.init(allocator);
        runStdJson_ArrayTraversal(&adapter, &parsed.value);
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchStdJson_DeepComponentParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_text, .{ .parse_numbers = false });
        defer parsed.deinit();
        var adapter = StdJsonAdapter.init(allocator);
        runStdJson_DeepComponent(&adapter, &parsed.value);
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchStdJson_WhereFilterParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_text, .{ .parse_numbers = false });
        defer parsed.deinit();
        var adapter = StdJsonAdapter.init(allocator);
        runStdJson_WhereFilter(&adapter, &parsed.value);
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchStdJson_CountParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_text, .{ .parse_numbers = false });
        defer parsed.deinit();
        var adapter = StdJsonAdapter.init(allocator);
        runStdJson_Count(&adapter, &parsed.value);
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

fn benchStdJson_BundleTraversalParseEach(allocator: std.mem.Allocator, json_text: []const u8, iterations: usize) !u64 {
    const start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_text, .{ .parse_numbers = false });
        defer parsed.deinit();
        var adapter = StdJsonAdapter.init(allocator);
        runStdJson_BundleTraversal(&adapter, &parsed.value);
    }
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

// ============================================================================
// Main
// ============================================================================

fn printResult(name: []const u8, jdoc_ns: u64, stdjson_ns: u64, iterations: usize) void {
    const jdoc_ms = @as(f64, @floatFromInt(jdoc_ns)) / 1_000_000.0;
    const stdjson_ms = @as(f64, @floatFromInt(stdjson_ns)) / 1_000_000.0;
    const ratio = @as(f64, @floatFromInt(stdjson_ns)) / @as(f64, @floatFromInt(jdoc_ns));
    const jdoc_ops = @as(f64, @floatFromInt(iterations)) / (@as(f64, @floatFromInt(jdoc_ns)) / 1_000_000_000.0);
    
    std.debug.print("{s:<35} JsonDoc: {d:>7.2}ms  StdJson: {d:>7.2}ms  ratio: {d:.2}x  ({d:.0} ops/s)\n", .{
        name, jdoc_ms, stdjson_ms, ratio, jdoc_ops,
    });
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
    std.debug.print("=" ** 80 ++ "\n", .{});
    std.debug.print("NodeAdapter Performance Benchmark - Realistic FHIRPath Patterns\n", .{});
    std.debug.print("=" ** 80 ++ "\n\n", .{});

    // Generate bundle
    const bundle_json = try generateBundle(allocator, bundle_size);
    defer allocator.free(bundle_json);

    std.debug.print("Test Data:\n", .{});
    std.debug.print("  Patient JSON:     {d:>6} bytes\n", .{PATIENT_JSON.len});
    std.debug.print("  Observation JSON: {d:>6} bytes\n", .{OBSERVATION_JSON.len});
    std.debug.print("  Bundle JSON:      {d:>6} bytes ({d} entries)\n", .{bundle_json.len, bundle_size});
    std.debug.print("  Iterations:       {d:>6}\n\n", .{iterations});
    std.debug.print("  Mode:             {s}\n\n", .{if (options.parse_each) "parse-each (parse+eval)" else "parse-once (navigation only)"});

    // Run benchmarks
    std.debug.print("Benchmark                            JsonDoc         StdJson         Ratio   Throughput\n", .{});
    std.debug.print("-" ** 95 ++ "\n", .{});

    if (options.parse_each) {
        {
            const jdoc_ns = try benchJsonDoc_SimpleNestedParseEach(allocator, OBSERVATION_JSON, iterations);
            const stdjson_ns = try benchStdJson_SimpleNestedParseEach(allocator, OBSERVATION_JSON, iterations);
            printResult("code.coding.code (Observation)", jdoc_ns, stdjson_ns, iterations);
        }
        {
            const jdoc_ns = try benchJsonDoc_ArrayTraversalParseEach(allocator, PATIENT_JSON, iterations);
            const stdjson_ns = try benchStdJson_ArrayTraversalParseEach(allocator, PATIENT_JSON, iterations);
            printResult("name.given (Patient)", jdoc_ns, stdjson_ns, iterations);
        }
        {
            const jdoc_ns = try benchJsonDoc_DeepComponentParseEach(allocator, OBSERVATION_JSON, iterations);
            const stdjson_ns = try benchStdJson_DeepComponentParseEach(allocator, OBSERVATION_JSON, iterations);
            printResult("component.code.coding.code (Obs)", jdoc_ns, stdjson_ns, iterations);
        }
        {
            const jdoc_ns = try benchJsonDoc_WhereFilterParseEach(allocator, PATIENT_JSON, iterations);
            const stdjson_ns = try benchStdJson_WhereFilterParseEach(allocator, PATIENT_JSON, iterations);
            printResult("identifier.where(sys=X).value (Pt)", jdoc_ns, stdjson_ns, iterations);
        }
        {
            const jdoc_ns = try benchJsonDoc_CountParseEach(allocator, PATIENT_JSON, iterations);
            const stdjson_ns = try benchStdJson_CountParseEach(allocator, PATIENT_JSON, iterations);
            printResult("name.given.count() (Patient)", jdoc_ns, stdjson_ns, iterations);
        }
        {
            const jdoc_ns = try benchJsonDoc_BundleTraversalParseEach(allocator, bundle_json, bundle_iters);
            const stdjson_ns = try benchStdJson_BundleTraversalParseEach(allocator, bundle_json, bundle_iters);
            printResult("Bundle filter+traverse (100 entries)", jdoc_ns, stdjson_ns, bundle_iters);
        }
    } else {
        // Parse all documents
        var patient_jdoc = try jsondoc.JsonDoc.init(allocator, PATIENT_JSON);
        defer patient_jdoc.deinit();
        var patient_jdoc_adapter = JsonDocAdapter.init(&patient_jdoc);
        
        const patient_stdjson = try std.json.parseFromSlice(std.json.Value, allocator, PATIENT_JSON, .{ .parse_numbers = false });
        defer patient_stdjson.deinit();
        var patient_stdjson_adapter = StdJsonAdapter.init(allocator);

        var obs_jdoc = try jsondoc.JsonDoc.init(allocator, OBSERVATION_JSON);
        defer obs_jdoc.deinit();
        var obs_jdoc_adapter = JsonDocAdapter.init(&obs_jdoc);
        
        const obs_stdjson = try std.json.parseFromSlice(std.json.Value, allocator, OBSERVATION_JSON, .{ .parse_numbers = false });
        defer obs_stdjson.deinit();
        var obs_stdjson_adapter = StdJsonAdapter.init(allocator);

        var bundle_jdoc = try jsondoc.JsonDoc.init(allocator, bundle_json);
        defer bundle_jdoc.deinit();
        var bundle_jdoc_adapter = JsonDocAdapter.init(&bundle_jdoc);
        
        const bundle_stdjson = try std.json.parseFromSlice(std.json.Value, allocator, bundle_json, .{ .parse_numbers = false });
        defer bundle_stdjson.deinit();
        var bundle_stdjson_adapter = StdJsonAdapter.init(allocator);

        {
            const jdoc_ns = benchJsonDoc_SimpleNested(&obs_jdoc_adapter, obs_jdoc_adapter.root(), iterations);
            const stdjson_ns = benchStdJson_SimpleNested(&obs_stdjson_adapter, &obs_stdjson.value, iterations);
            printResult("code.coding.code (Observation)", jdoc_ns, stdjson_ns, iterations);
        }
        {
            const jdoc_ns = benchJsonDoc_ArrayTraversal(&patient_jdoc_adapter, patient_jdoc_adapter.root(), iterations);
            const stdjson_ns = benchStdJson_ArrayTraversal(&patient_stdjson_adapter, &patient_stdjson.value, iterations);
            printResult("name.given (Patient)", jdoc_ns, stdjson_ns, iterations);
        }
        {
            const jdoc_ns = benchJsonDoc_DeepComponent(&obs_jdoc_adapter, obs_jdoc_adapter.root(), iterations);
            const stdjson_ns = benchStdJson_DeepComponent(&obs_stdjson_adapter, &obs_stdjson.value, iterations);
            printResult("component.code.coding.code (Obs)", jdoc_ns, stdjson_ns, iterations);
        }
        {
            const jdoc_ns = benchJsonDoc_WhereFilter(&patient_jdoc_adapter, patient_jdoc_adapter.root(), iterations);
            const stdjson_ns = benchStdJson_WhereFilter(&patient_stdjson_adapter, &patient_stdjson.value, iterations);
            printResult("identifier.where(sys=X).value (Pt)", jdoc_ns, stdjson_ns, iterations);
        }
        {
            const jdoc_ns = benchJsonDoc_Count(&patient_jdoc_adapter, patient_jdoc_adapter.root(), iterations);
            const stdjson_ns = benchStdJson_Count(&patient_stdjson_adapter, &patient_stdjson.value, iterations);
            printResult("name.given.count() (Patient)", jdoc_ns, stdjson_ns, iterations);
        }
        {
            const jdoc_ns = benchJsonDoc_BundleTraversal(&bundle_jdoc_adapter, bundle_jdoc_adapter.root(), bundle_iters);
            const stdjson_ns = benchStdJson_BundleTraversal(&bundle_stdjson_adapter, &bundle_stdjson.value, bundle_iters);
            printResult("Bundle filter+traverse (100 entries)", jdoc_ns, stdjson_ns, bundle_iters);
        }
    }

    std.debug.print("\n", .{});
    std.debug.print("Note: ratio > 1.0 means JsonDoc is faster than StdJson\n", .{});
    std.debug.print("Sink: {d} (prevents optimization)\n", .{sink});
}
