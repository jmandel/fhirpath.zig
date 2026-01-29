const std = @import("std");

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

/// Well-known System type IDs. These match the deterministic init order in TypeTable.init().
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
};

pub const TypeTable = struct {
    allocator: std.mem.Allocator,
    map: std.StringHashMap(u32),
    names: std.ArrayListUnmanaged([]const u8) = .{},

    pub fn init(allocator: std.mem.Allocator) !TypeTable {
        var table = TypeTable{
            .allocator = allocator,
            .map = std.StringHashMap(u32).init(allocator),
        };
        _ = try table.getOrAdd("System.Any");
        _ = try table.getOrAdd("System.Boolean");
        _ = try table.getOrAdd("System.Integer");
        _ = try table.getOrAdd("System.Long");
        _ = try table.getOrAdd("System.Decimal");
        _ = try table.getOrAdd("System.String");
        _ = try table.getOrAdd("System.Date");
        _ = try table.getOrAdd("System.DateTime");
        _ = try table.getOrAdd("System.Time");
        _ = try table.getOrAdd("System.Quantity");
        return table;
    }

    pub fn deinit(self: *TypeTable) void {
        for (self.names.items) |type_name| {
            self.allocator.free(type_name);
        }
        self.map.deinit();
        self.names.deinit(self.allocator);
    }

    pub fn getOrAdd(self: *TypeTable, type_name: []const u8) !u32 {
        if (self.map.get(type_name)) |id| return id;
        const owned = try self.allocator.dupe(u8, type_name);
        const id: u32 = @intCast(self.names.items.len + 1);
        try self.names.append(self.allocator, owned);
        try self.map.put(owned, id);
        return id;
    }

    pub fn name(self: *TypeTable, id: u32) []const u8 {
        if (id == 0) return "";
        const idx: usize = @intCast(id - 1);
        if (idx >= self.names.items.len) return "";
        return self.names.items[idx];
    }
};
