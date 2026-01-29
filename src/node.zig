//! NodeAdapter interface for generic JSON-like document traversal.
//!
//! This module defines the compile-time interface that any JSON backend must implement.
//! The evaluator is generic over the adapter type, allowing different backends:
//! - JsonDocAdapter: our custom DOM with span preservation
//! - StdJsonAdapter: std.json.Value for compatibility
//! - Future: WASM host adapter for direct JS object traversal
//!
//! All adapter calls are inlined at compile time - no vtables or runtime dispatch.

const std = @import("std");

/// Node kinds that any adapter must support
pub const Kind = enum {
    object,
    array,
    string,
    number,
    bool,
    null,
};

/// Span into the original input buffer (for zero-copy results)
pub const Span = struct {
    pos: u32,
    end: u32,
};

/// Object field entry for iteration
pub fn ObjectEntry(comptime NodeRef: type) type {
    return struct {
        key: []const u8,
        value: NodeRef,
    };
}

/// Compile-time interface validation for NodeAdapter implementations
pub fn requireAdapter(comptime A: type) void {
    comptime {
        // Must have NodeRef type
        if (!@hasDecl(A, "NodeRef")) {
            @compileError("NodeAdapter must declare NodeRef type");
        }

        // Core navigation methods
        for (.{ "kind", "objectGet", "objectCount", "arrayLen", "arrayAt" }) |name| {
            if (!@hasDecl(A, name)) {
                @compileError("NodeAdapter missing navigation method: " ++ name);
            }
        }

        // Scalar accessors
        for (.{ "string", "numberText", "boolean" }) |name| {
            if (!@hasDecl(A, name)) {
                @compileError("NodeAdapter missing scalar accessor: " ++ name);
            }
        }

        // Object iteration (needed for deep equality, stringify)
        if (!@hasDecl(A, "ObjectIter")) {
            @compileError("NodeAdapter must declare ObjectIter type");
        }
        if (!@hasDecl(A, "objectIter")) {
            @compileError("NodeAdapter must have objectIter() method");
        }

        // Type-aware value conversion
        if (!@hasDecl(A, "toValue")) {
            @compileError("NodeAdapter must have toValue() method");
        }
    }
}

/// Check if adapter has a root() method (optional - StdJsonAdapter doesn't need it)
pub fn hasRootMethod(comptime A: type) bool {
    return @hasDecl(A, "root");
}

/// Check if adapter supports span access (optional capability)
pub fn hasSpanSupport(comptime A: type) bool {
    return @hasDecl(A, "span");
}

/// Check if adapter supports direct stringify (optional capability)
pub fn hasStringifySupport(comptime A: type) bool {
    return @hasDecl(A, "stringify");
}

/// Deep equality comparison for nodes (generic over adapter)
pub fn nodeEqual(comptime A: type, adapter: *A, a: A.NodeRef, b: A.NodeRef) bool {
    const ka = A.kind(adapter, a);
    const kb = A.kind(adapter, b);

    if (ka != kb) {
        // Special case: number comparison across int/float representations
        if (ka == .number and kb == .number) {
            return numberTextEqual(A.numberText(adapter, a), A.numberText(adapter, b));
        }
        return false;
    }

    return switch (ka) {
        .null => true,
        .bool => A.boolean(adapter, a) == A.boolean(adapter, b),
        .string => std.mem.eql(u8, A.string(adapter, a), A.string(adapter, b)),
        .number => numberTextEqual(A.numberText(adapter, a), A.numberText(adapter, b)),
        .array => {
            const la = A.arrayLen(adapter, a);
            if (la != A.arrayLen(adapter, b)) return false;
            for (0..la) |i| {
                if (!nodeEqual(A, adapter, A.arrayAt(adapter, a, i), A.arrayAt(adapter, b, i))) {
                    return false;
                }
            }
            return true;
        },
        .object => {
            if (A.objectCount(adapter, a) != A.objectCount(adapter, b)) return false;

            var it = A.objectIter(adapter, a);
            while (it.next()) |entry| {
                const other = A.objectGet(adapter, b, entry.key) orelse return false;
                if (!nodeEqual(A, adapter, entry.value, other)) return false;
            }
            return true;
        },
    };
}

/// Number text comparison (handles leading zeros, decimal equivalence)
fn numberTextEqual(a: []const u8, b: []const u8) bool {
    // Simple text comparison for now - can be enhanced for decimal semantics
    return std.mem.eql(u8, a, b);
}
