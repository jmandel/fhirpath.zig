//! Verify that JsonDocAdapter and StdJsonAdapter produce identical results
const std = @import("std");
const jsondoc = @import("jsondoc.zig");
const node = @import("node.zig");
const JsonDocAdapter = @import("backends/jsondoc.zig").JsonDocAdapter;
const StdJsonAdapter = @import("backends/stdjson.zig").StdJsonAdapter;

const TEST_JSON =
    \\{
    \\  "resourceType": "Patient",
    \\  "id": "example",
    \\  "identifier": [
    \\    {"system": "urn:oid:1.2.36.146.595.217.0.1", "value": "12345"},
    \\    {"system": "urn:foo", "value": "foo-value"}
    \\  ],
    \\  "name": [
    \\    {"family": "Chalmers", "given": ["Peter", "James"]},
    \\    {"family": "Windsor", "given": ["Jim"]}
    \\  ],
    \\  "active": true,
    \\  "birthDate": "1974-12-25"
    \\}
;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    std.debug.print("\n=== Adapter Correctness Verification ===\n\n", .{});

    // Parse with both backends
    var jdoc = try jsondoc.JsonDoc.init(allocator, TEST_JSON);
    defer jdoc.deinit();
    var jdoc_adapter = JsonDocAdapter.init(&jdoc);

    const stdjson_parsed = try std.json.parseFromSlice(std.json.Value, allocator, TEST_JSON, .{ .parse_numbers = false });
    defer stdjson_parsed.deinit();
    var stdjson_adapter = StdJsonAdapter.init(allocator);

    var passed: usize = 0;
    var failed: usize = 0;

    // Test 1: resourceType
    {
        const jdoc_val = jdoc_adapter.objectGet(jdoc_adapter.root(), "resourceType");
        const stdjson_val = stdjson_adapter.objectGet(&stdjson_parsed.value, "resourceType");
        if (jdoc_val != null and stdjson_val != null) {
            const j = jdoc_adapter.string(jdoc_val.?);
            const s = stdjson_adapter.string(stdjson_val.?);
            if (std.mem.eql(u8, j, s)) {
                std.debug.print("PASS resourceType: '{s}'\n", .{j});
                passed += 1;
            } else {
                std.debug.print("FAIL resourceType: JsonDoc='{s}', StdJson='{s}'\n", .{j, s});
                failed += 1;
            }
        } else {
            std.debug.print("FAIL resourceType: null result\n", .{});
            failed += 1;
        }
    }

    // Test 2: name[0].family
    {
        const jdoc_names = jdoc_adapter.objectGet(jdoc_adapter.root(), "name");
        const stdjson_names = stdjson_adapter.objectGet(&stdjson_parsed.value, "name");
        if (jdoc_names != null and stdjson_names != null) {
            const jdoc_first = jdoc_adapter.arrayAt(jdoc_names.?, 0);
            const stdjson_first = stdjson_adapter.arrayAt(stdjson_names.?, 0);
            const jdoc_family = jdoc_adapter.objectGet(jdoc_first, "family");
            const stdjson_family = stdjson_adapter.objectGet(stdjson_first, "family");
            if (jdoc_family != null and stdjson_family != null) {
                const j = jdoc_adapter.string(jdoc_family.?);
                const s = stdjson_adapter.string(stdjson_family.?);
                if (std.mem.eql(u8, j, s)) {
                    std.debug.print("PASS name[0].family: '{s}'\n", .{j});
                    passed += 1;
                } else {
                    std.debug.print("FAIL name[0].family: JsonDoc='{s}', StdJson='{s}'\n", .{j, s});
                    failed += 1;
                }
            }
        }
    }

    // Test 3: name[0].given[0]
    {
        const jdoc_names = jdoc_adapter.objectGet(jdoc_adapter.root(), "name");
        const stdjson_names = stdjson_adapter.objectGet(&stdjson_parsed.value, "name");
        if (jdoc_names != null and stdjson_names != null) {
            const jdoc_first = jdoc_adapter.arrayAt(jdoc_names.?, 0);
            const stdjson_first = stdjson_adapter.arrayAt(stdjson_names.?, 0);
            const jdoc_given = jdoc_adapter.objectGet(jdoc_first, "given");
            const stdjson_given = stdjson_adapter.objectGet(stdjson_first, "given");
            if (jdoc_given != null and stdjson_given != null) {
                const jdoc_g0 = jdoc_adapter.arrayAt(jdoc_given.?, 0);
                const stdjson_g0 = stdjson_adapter.arrayAt(stdjson_given.?, 0);
                const j = jdoc_adapter.string(jdoc_g0);
                const s = stdjson_adapter.string(stdjson_g0);
                if (std.mem.eql(u8, j, s)) {
                    std.debug.print("PASS name[0].given[0]: '{s}'\n", .{j});
                    passed += 1;
                } else {
                    std.debug.print("FAIL name[0].given[0]: JsonDoc='{s}', StdJson='{s}'\n", .{j, s});
                    failed += 1;
                }
            }
        }
    }

    // Test 4: active (boolean)
    {
        const jdoc_val = jdoc_adapter.objectGet(jdoc_adapter.root(), "active");
        const stdjson_val = stdjson_adapter.objectGet(&stdjson_parsed.value, "active");
        if (jdoc_val != null and stdjson_val != null) {
            const j = jdoc_adapter.boolean(jdoc_val.?);
            const s = stdjson_adapter.boolean(stdjson_val.?);
            if (j == s) {
                std.debug.print("PASS active: {}\n", .{j});
                passed += 1;
            } else {
                std.debug.print("FAIL active: JsonDoc={}, StdJson={}\n", .{j, s});
                failed += 1;
            }
        }
    }

    // Test 5: identifier array length
    {
        const jdoc_ids = jdoc_adapter.objectGet(jdoc_adapter.root(), "identifier");
        const stdjson_ids = stdjson_adapter.objectGet(&stdjson_parsed.value, "identifier");
        if (jdoc_ids != null and stdjson_ids != null) {
            const j = jdoc_adapter.arrayLen(jdoc_ids.?);
            const s = stdjson_adapter.arrayLen(stdjson_ids.?);
            if (j == s) {
                std.debug.print("PASS identifier.length: {d}\n", .{j});
                passed += 1;
            } else {
                std.debug.print("FAIL identifier.length: JsonDoc={d}, StdJson={d}\n", .{j, s});
                failed += 1;
            }
        }
    }

    // Test 6: identifier[1].value (the "urn:foo" one)
    {
        const jdoc_ids = jdoc_adapter.objectGet(jdoc_adapter.root(), "identifier");
        const stdjson_ids = stdjson_adapter.objectGet(&stdjson_parsed.value, "identifier");
        if (jdoc_ids != null and stdjson_ids != null) {
            const jdoc_id1 = jdoc_adapter.arrayAt(jdoc_ids.?, 1);
            const stdjson_id1 = stdjson_adapter.arrayAt(stdjson_ids.?, 1);
            const jdoc_val = jdoc_adapter.objectGet(jdoc_id1, "value");
            const stdjson_val = stdjson_adapter.objectGet(stdjson_id1, "value");
            if (jdoc_val != null and stdjson_val != null) {
                const j = jdoc_adapter.string(jdoc_val.?);
                const s = stdjson_adapter.string(stdjson_val.?);
                if (std.mem.eql(u8, j, s)) {
                    std.debug.print("PASS identifier[1].value: '{s}'\n", .{j});
                    passed += 1;
                } else {
                    std.debug.print("FAIL identifier[1].value: JsonDoc='{s}', StdJson='{s}'\n", .{j, s});
                    failed += 1;
                }
            }
        }
    }

    // Test 7: kind() for various nodes
    {
        const jdoc_root_kind = jdoc_adapter.kind(jdoc_adapter.root());
        const stdjson_root_kind = stdjson_adapter.kind(&stdjson_parsed.value);
        if (jdoc_root_kind == stdjson_root_kind and jdoc_root_kind == .object) {
            std.debug.print("PASS root kind: .object\n", .{});
            passed += 1;
        } else {
            std.debug.print("FAIL root kind: JsonDoc={}, StdJson={}\n", .{jdoc_root_kind, stdjson_root_kind});
            failed += 1;
        }
    }

    std.debug.print("\n=== Summary: {d} passed, {d} failed ===\n\n", .{passed, failed});
    
    if (failed > 0) {
        std.process.exit(1);
    }
}
