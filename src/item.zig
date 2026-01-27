const std = @import("std");

pub const DataKind = enum(u32) {
    none = 0,
    json_span = 1,
    value = 2,
};

pub const ValueKind = enum(u32) {
    empty,
    boolean,
    integer,
    decimal,
    string,
    date,
    time,
    dateTime,
    quantity,
};

pub const Quantity = struct {
    value: []const u8,
    unit: []const u8,
};

pub const Value = union(ValueKind) {
    empty: void,
    boolean: bool,
    integer: i64,
    decimal: []const u8,
    string: []const u8,
    date: []const u8,
    time: []const u8,
    dateTime: []const u8,
    quantity: Quantity,
};

pub const Item = struct {
    data_kind: DataKind,
    value_kind: ValueKind,
    type_id: u32,
    source_pos: u32,
    source_end: u32,
    data_pos: u32,
    data_end: u32,
    node: ?u32,
    value: ?Value,
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
