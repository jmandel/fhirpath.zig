pub const DataKind = enum(u32) {
    none = 0,
    // Node-backed item. Spans are optional and depend on adapter support.
    node_ref = 1,
    value = 2,
};

pub const ValueKind = enum(u32) {
    empty,
    boolean,
    integer,
    long, // System.Long (64-bit integer with 'L' suffix)
    decimal,
    string,
    date,
    time,
    dateTime,
    quantity,
    typeInfo, // Reflection: SimpleTypeInfo with namespace and name
};

pub const Quantity = struct {
    value: []const u8,
    unit: []const u8,
};

/// SimpleTypeInfo for reflection - stores namespace and name.
/// Used by type() function to return type information.
pub const TypeInfo = struct {
    namespace: []const u8, // e.g., "System" or "FHIR"
    name: []const u8, // e.g., "String", "Integer", "Patient"
};

pub const Value = union(ValueKind) {
    empty: void,
    boolean: bool,
    integer: i64,
    long: i64, // System.Long (64-bit integer)
    decimal: []const u8,
    string: []const u8,
    date: []const u8,
    time: []const u8,
    dateTime: []const u8,
    quantity: Quantity,
    typeInfo: TypeInfo,
};

pub const Item = struct {
    data_kind: DataKind,
    value_kind: ValueKind,
    type_id: u32,
    source_pos: u32,
    source_end: u32,
    // Adapter-opaque node reference (index or pointer encoded as usize).
    // If the adapter supports spans, source_pos/source_end may be populated.
    node: ?usize,
    value: ?Value,
};

/// Well-known System type IDs (comptime constants matching schema.SystemTypeNames order).
pub const SystemTypeIds = struct {
    pub const any = 1;
    pub const boolean = 2;
    pub const integer = 3;
    pub const long = 4;
    pub const decimal = 5;
    pub const string = 6;
    pub const date = 7;
    pub const dateTime = 8;
    pub const time = 9;
    pub const quantity = 10;
    pub const typeInfo = 11;
};

