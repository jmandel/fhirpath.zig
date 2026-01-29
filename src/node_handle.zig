//! Unified node handle type for all adapters.
//!
//! A NodeHandle is a `usize` that can represent either:
//! - A pointer-backed node (LSB=0): the handle IS the pointer value
//! - An index-backed virtual/synthetic node (LSB=1): index = handle >> 1
//!
//! This allows adapters to mix pointer-fast access to underlying data
//! (e.g., *const std.json.Value) with virtual nodes (e.g., merged FHIR
//! primitives, synthetic output values) in a single handle space.

/// Opaque node reference used by all adapters.
pub const NodeHandle = usize;

/// Returns true if this handle refers to a virtual/index-backed node.
pub fn isIndex(h: NodeHandle) bool {
    return (h & 1) == 1;
}

/// Extract the virtual node index from an index-backed handle.
pub fn toIndex(h: NodeHandle) usize {
    return h >> 1;
}

/// Create a handle from a virtual node index.
pub fn fromIndex(i: usize) NodeHandle {
    return (i << 1) | 1;
}

/// Create a handle from a pointer to an underlying node.
/// Assumes at least 2-byte alignment (true for normal allocations).
pub fn fromPtr(comptime T: type, p: *const T) NodeHandle {
    return @intFromPtr(p);
}

/// Recover a pointer from a pointer-backed handle.
pub fn toPtr(comptime T: type, h: NodeHandle) *const T {
    return @ptrFromInt(h);
}
