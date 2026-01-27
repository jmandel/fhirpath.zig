//! Benchmark adapter overhead vs direct JsonDoc access
//! This measures if the adapter abstraction adds any overhead
const std = @import("std");
const jsondoc = @import("jsondoc.zig");
const JsonDocAdapter = @import("backends/jsondoc.zig").JsonDocAdapter;

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
    } else if (T == usize or T == u64 or T == u32) {
        sink +%= val;
    } else if (@sizeOf(T) > 0) {
        sink +%= @sizeOf(T);
    }
}

const PATIENT_JSON =
    \\{
    \\  "resourceType": "Patient",
    \\  "id": "example",
    \\  "name": [
    \\    {"family": "Chalmers", "given": ["Peter", "James"]},
    \\    {"family": "Windsor", "given": ["Jim"]}
    \\  ],
    \\  "identifier": [
    \\    {"system": "urn:foo", "value": "12345"}
    \\  ]
    \\}
;

// Benchmark using JsonDocAdapter
fn benchAdapter(adapter: *JsonDocAdapter, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        const root = adapter.root();
        // Navigate: name[0].given[0]
        if (adapter.kind(root) == .object) {
            if (adapter.objectGet(root, "name")) |names| {
                if (adapter.kind(names) == .array and adapter.arrayLen(names) > 0) {
                    const first_name = adapter.arrayAt(names, 0);
                    if (adapter.kind(first_name) == .object) {
                        if (adapter.objectGet(first_name, "given")) |given| {
                            if (adapter.kind(given) == .array and adapter.arrayLen(given) > 0) {
                                const first_given = adapter.arrayAt(given, 0);
                                doNotOptimize(adapter.string(first_given));
                            }
                        }
                    }
                }
            }
        }
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

// Benchmark using direct JsonDoc access (like original ~/fhirpath.zig eval.zig)
fn benchDirect(doc: *jsondoc.JsonDoc, iterations: usize) u64 {
    const start = std.time.nanoTimestamp();
    
    for (0..iterations) |_| {
        const root_node = doc.node(doc.root).*;
        // Navigate: name[0].given[0]
        if (root_node.kind == .object) {
            for (root_node.data.object) |field| {
                if (std.mem.eql(u8, field.key, "name")) {
                    const names_node = doc.node(field.value).*;
                    if (names_node.kind == .array and names_node.data.array.len > 0) {
                        const first_name_node = doc.node(names_node.data.array[0]).*;
                        if (first_name_node.kind == .object) {
                            for (first_name_node.data.object) |inner_field| {
                                if (std.mem.eql(u8, inner_field.key, "given")) {
                                    const given_node = doc.node(inner_field.value).*;
                                    if (given_node.kind == .array and given_node.data.array.len > 0) {
                                        const first_given_node = doc.node(given_node.data.array[0]).*;
                                        doNotOptimize(first_given_node.data.string);
                                    }
                                }
                            }
                        }
                    }
                    break;
                }
            }
        }
    }
    
    return @intCast(@as(i64, @intCast(std.time.nanoTimestamp())) - @as(i64, @intCast(start)));
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    std.debug.print("\n", .{});
    std.debug.print("=" ** 70 ++ "\n", .{});
    std.debug.print("Adapter Overhead Benchmark: JsonDocAdapter vs Direct JsonDoc\n", .{});
    std.debug.print("=" ** 70 ++ "\n\n", .{});

    const iterations = 1_000_000;
    std.debug.print("Iterations: {d}\n", .{iterations});
    std.debug.print("Pattern: name[0].given[0]\n\n", .{});

    var jdoc = try jsondoc.JsonDoc.init(allocator, PATIENT_JSON);
    defer jdoc.deinit();
    var adapter = JsonDocAdapter.init(&jdoc);

    // Warm up
    _ = benchAdapter(&adapter, 1000);
    _ = benchDirect(&jdoc, 1000);

    // Run benchmarks multiple times
    var adapter_total: u64 = 0;
    var direct_total: u64 = 0;
    const runs = 5;

    for (0..runs) |i| {
        const adapter_ns = benchAdapter(&adapter, iterations);
        const direct_ns = benchDirect(&jdoc, iterations);
        adapter_total += adapter_ns;
        direct_total += direct_ns;
        
        std.debug.print("Run {d}: Adapter={d:.2}ms, Direct={d:.2}ms, ratio={d:.3}x\n", .{
            i + 1,
            @as(f64, @floatFromInt(adapter_ns)) / 1_000_000.0,
            @as(f64, @floatFromInt(direct_ns)) / 1_000_000.0,
            @as(f64, @floatFromInt(adapter_ns)) / @as(f64, @floatFromInt(direct_ns)),
        });
    }

    const adapter_avg = @as(f64, @floatFromInt(adapter_total)) / @as(f64, runs);
    const direct_avg = @as(f64, @floatFromInt(direct_total)) / @as(f64, runs);
    const overhead = (adapter_avg / direct_avg - 1.0) * 100.0;

    std.debug.print("\n--- Average ---\n", .{});
    std.debug.print("JsonDocAdapter: {d:.2} ms\n", .{adapter_avg / 1_000_000.0});
    std.debug.print("Direct JsonDoc: {d:.2} ms\n", .{direct_avg / 1_000_000.0});
    std.debug.print("Adapter overhead: {d:.1}%\n", .{overhead});
    
    if (overhead < 5.0) {
        std.debug.print("\nConclusion: Adapter abstraction has NEGLIGIBLE overhead (<5%)\n", .{});
    } else if (overhead < 20.0) {
        std.debug.print("\nConclusion: Adapter has SMALL overhead ({d:.1}%)\n", .{overhead});
    } else {
        std.debug.print("\nConclusion: Adapter has SIGNIFICANT overhead ({d:.1}%)\n", .{overhead});
    }

    std.debug.print("\nSink: {d}\n", .{sink});
}
