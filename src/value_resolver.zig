//! Centralized type-aware value conversion.
//!
//! Converts adapter node references to `item.Value` based on type_id and
//! optional schema information. This logic was previously embedded in the
//! adapter (toValue), but belongs in the engine layer since it requires
//! schema/type knowledge that adapters shouldn't own.

const std = @import("std");
const item = @import("item.zig");
const schema_mod = @import("schema.zig");
const node_mod = @import("node.zig");

/// Convert an adapter node to a typed Value.
///
/// Resolution order:
/// 1. Schema-aware: if schema is present and type_id maps to a FHIR model type
///    with an implicit system type, use that system type's conversion.
/// 2. Well-known System type IDs (date, dateTime, time) from TypeTable.
/// 3. JSON-kind fallback: infer from the node's structural kind.
pub fn nodeToValue(
    comptime A: type,
    adapter: *A,
    ref: A.NodeRef,
    type_id: u32,
    schema_ptr: ?*schema_mod.Schema,
) item.Value {
    // Schema-aware path (FHIR flavor with schema)
    if (schema_ptr) |s| {
        if (schema_mod.isModelType(type_id)) {
            const sys_id = s.implicitSystemTypeId(type_id);
            if (sys_id != 0) {
                const sys_name = schema_mod.systemTypeName(sys_id);
                return toValueForSystemType(A, adapter, ref, sys_name);
            }
        }
    }
    // Well-known System type IDs
    if (type_id == item.SystemTypeIds.date) {
        if (A.kind(adapter, ref) == .string) return .{ .date = A.string(adapter, ref) };
    } else if (type_id == item.SystemTypeIds.dateTime) {
        if (A.kind(adapter, ref) == .string) return .{ .dateTime = A.string(adapter, ref) };
    } else if (type_id == item.SystemTypeIds.time) {
        if (A.kind(adapter, ref) == .string) return .{ .time = A.string(adapter, ref) };
    }
    // JSON-kind-based fallback
    return toValueFromJsonKind(A, adapter, ref);
}

fn toValueForSystemType(
    comptime A: type,
    adapter: *A,
    ref: A.NodeRef,
    sys_name: []const u8,
) item.Value {
    if (std.mem.eql(u8, sys_name, "System.Date")) {
        if (A.kind(adapter, ref) == .string) return .{ .date = A.string(adapter, ref) };
        return toValueFromJsonKind(A, adapter, ref);
    }
    if (std.mem.eql(u8, sys_name, "System.DateTime")) {
        if (A.kind(adapter, ref) == .string) return .{ .dateTime = A.string(adapter, ref) };
        return toValueFromJsonKind(A, adapter, ref);
    }
    if (std.mem.eql(u8, sys_name, "System.Time")) {
        if (A.kind(adapter, ref) == .string) return .{ .time = A.string(adapter, ref) };
        return toValueFromJsonKind(A, adapter, ref);
    }
    if (std.mem.eql(u8, sys_name, "System.Quantity")) {
        return extractQuantity(A, adapter, ref);
    }
    // Boolean — trust schema over kind()
    if (std.mem.eql(u8, sys_name, "System.Boolean")) {
        return switch (A.kind(adapter, ref)) {
            .bool => .{ .boolean = A.boolean(adapter, ref) },
            .string => .{ .boolean = std.mem.eql(u8, A.string(adapter, ref), "true") },
            else => toValueFromJsonKind(A, adapter, ref),
        };
    }
    // Integer — accept both .number and .string (XML uses value attr)
    if (std.mem.eql(u8, sys_name, "System.Integer")) {
        const text = switch (A.kind(adapter, ref)) {
            .number => A.numberText(adapter, ref),
            .string => A.string(adapter, ref),
            else => return toValueFromJsonKind(A, adapter, ref),
        };
        const parsed = std.fmt.parseInt(i64, text, 10) catch return .{ .decimal = text };
        return .{ .integer = parsed };
    }
    // Decimal — accept both .number and .string (XML uses value attr)
    if (std.mem.eql(u8, sys_name, "System.Decimal")) {
        const text = switch (A.kind(adapter, ref)) {
            .number => A.numberText(adapter, ref),
            .string => A.string(adapter, ref),
            else => return toValueFromJsonKind(A, adapter, ref),
        };
        if (isIntegerText(text)) {
            const parsed = std.fmt.parseInt(i64, text, 10) catch return .{ .decimal = text };
            return .{ .integer = parsed };
        }
        return .{ .decimal = text };
    }
    // String — extract directly
    if (std.mem.eql(u8, sys_name, "System.String")) {
        return .{ .string = A.string(adapter, ref) };
    }
    // Fallback
    return toValueFromJsonKind(A, adapter, ref);
}

fn extractQuantity(
    comptime A: type,
    adapter: *A,
    ref: A.NodeRef,
) item.Value {
    if (A.kind(adapter, ref) != .object) return toValueFromJsonKind(A, adapter, ref);

    const value_ref = A.objectGet(adapter, ref, "value") orelse return .{ .empty = {} };
    const value_text = switch (A.kind(adapter, value_ref)) {
        .number => A.numberText(adapter, value_ref),
        .string => A.string(adapter, value_ref),
        else => return .{ .empty = {} },
    };

    var unit_text: []const u8 = "1";
    if (A.objectGet(adapter, ref, "code")) |code_ref| {
        if (A.kind(adapter, code_ref) == .string) {
            unit_text = A.string(adapter, code_ref);
        }
    } else if (A.objectGet(adapter, ref, "unit")) |unit_ref| {
        if (A.kind(adapter, unit_ref) == .string) {
            unit_text = A.string(adapter, unit_ref);
        }
    }

    return .{ .quantity = .{ .value = value_text, .unit = unit_text } };
}

fn toValueFromJsonKind(
    comptime A: type,
    adapter: *A,
    ref: A.NodeRef,
) item.Value {
    return switch (A.kind(adapter, ref)) {
        .null => .{ .empty = {} },
        .bool => .{ .boolean = A.boolean(adapter, ref) },
        .number => {
            const text = A.numberText(adapter, ref);
            if (isIntegerText(text)) {
                const parsed = std.fmt.parseInt(i64, text, 10) catch return .{ .decimal = text };
                return .{ .integer = parsed };
            }
            return .{ .decimal = text };
        },
        .string => .{ .string = A.string(adapter, ref) },
        else => .{ .empty = {} },
    };
}

fn isIntegerText(text: []const u8) bool {
    return std.mem.indexOfScalar(u8, text, '.') == null and
        std.mem.indexOfAny(u8, text, "eE") == null;
}
