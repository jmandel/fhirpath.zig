const std = @import("std");

pub const NodeIndex = u32;

pub const NodeKind = enum {
    object,
    array,
    string,
    number,
    bool,
    null,
};

pub const Field = struct {
    key: []const u8,
    value: NodeIndex,
};

pub const Node = struct {
    kind: NodeKind,
    start: u32,
    end: u32,
    data: Data,

    pub const Data = union(NodeKind) {
        object: []Field,
        array: []NodeIndex,
        string: []const u8,
        number: []const u8,
        bool: bool,
        null: void,
    };
};

pub const ParseError = error{
    UnexpectedEnd,
    TrailingData,
    InvalidJson,
    InvalidEscape,
    InvalidNumber,
    InvalidBoolean,
    InvalidNull,
    OutOfMemory,
};

pub const JsonDoc = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    text: []const u8,
    nodes: std.ArrayListUnmanaged(Node) = .{},
    root: NodeIndex,

    pub fn init(allocator: std.mem.Allocator, text: []const u8) ParseError!JsonDoc {
        var doc = JsonDoc{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .text = text,
            .root = 0,
        };
        var parser = Parser.init(&doc, text);
        doc.root = try parser.parseValue();
        parser.skipWhitespace();
        if (parser.idx != text.len) return error.TrailingData;
        return doc;
    }

    pub fn deinit(self: *JsonDoc) void {
        self.nodes.deinit(self.allocator);
        self.arena.deinit();
    }

    pub fn node(self: *JsonDoc, idx: NodeIndex) *Node {
        return &self.nodes.items[idx];
    }
};

const Parser = struct {
    doc: *JsonDoc,
    text: []const u8,
    idx: usize,

    fn init(doc: *JsonDoc, text: []const u8) Parser {
        return .{ .doc = doc, .text = text, .idx = 0 };
    }

    fn parseValue(self: *Parser) ParseError!NodeIndex {
        self.skipWhitespace();
        if (self.idx >= self.text.len) return error.UnexpectedEnd;
        const c = self.text[self.idx];
        if (c == '{') return self.parseObject();
        if (c == '[') return self.parseArray();
        if (c == '"') return self.parseStringNode();
        if (c == 't' or c == 'f') return self.parseBool();
        if (c == 'n') return self.parseNull();
        return self.parseNumber();
    }

    fn parseObject(self: *Parser) ParseError!NodeIndex {
        const start = self.idx;
        try self.expectChar('{');
        self.skipWhitespace();
        var fields = std.ArrayListUnmanaged(Field){};
        defer fields.deinit(self.doc.arena.allocator());

        if (self.peekChar('}')) {
            self.idx += 1;
            return self.addNode(.object, start, self.idx, .{ .object = &[_]Field{} });
        }

        while (true) {
            self.skipWhitespace();
            const key = try self.parseString();
            self.skipWhitespace();
            try self.expectChar(':');
            const value_idx = try self.parseValue();
            try fields.append(self.doc.arena.allocator(), .{ .key = key, .value = value_idx });
            self.skipWhitespace();
            if (self.peekChar('}')) {
                self.idx += 1;
                break;
            }
            try self.expectChar(',');
        }

        const slice = try fields.toOwnedSlice(self.doc.arena.allocator());
        return self.addNode(.object, start, self.idx, .{ .object = slice });
    }

    fn parseArray(self: *Parser) ParseError!NodeIndex {
        const start = self.idx;
        try self.expectChar('[');
        self.skipWhitespace();
        var items = std.ArrayListUnmanaged(NodeIndex){};
        defer items.deinit(self.doc.arena.allocator());

        if (self.peekChar(']')) {
            self.idx += 1;
            return self.addNode(.array, start, self.idx, .{ .array = &[_]NodeIndex{} });
        }

        while (true) {
            const value_idx = try self.parseValue();
            try items.append(self.doc.arena.allocator(), value_idx);
            self.skipWhitespace();
            if (self.peekChar(']')) {
                self.idx += 1;
                break;
            }
            try self.expectChar(',');
        }

        const slice = try items.toOwnedSlice(self.doc.arena.allocator());
        return self.addNode(.array, start, self.idx, .{ .array = slice });
    }

    fn parseStringNode(self: *Parser) ParseError!NodeIndex {
        const start = self.idx;
        const value = try self.parseString();
        const end = self.idx;
        return self.addNode(.string, start, end, .{ .string = value });
    }

    fn parseString(self: *Parser) ParseError![]const u8 {
        try self.expectChar('"');
        var buf = std.ArrayListUnmanaged(u8){};
        while (self.idx < self.text.len) {
            const c = self.text[self.idx];
            if (c == '"') {
                self.idx += 1;
                return buf.toOwnedSlice(self.doc.arena.allocator());
            }
            if (c == '\\') {
                self.idx += 1;
                if (self.idx >= self.text.len) return error.UnexpectedEnd;
                const esc = self.text[self.idx];
                switch (esc) {
                    '"' => try buf.append(self.doc.arena.allocator(), '"'),
                    '\\' => try buf.append(self.doc.arena.allocator(), '\\'),
                    '/' => try buf.append(self.doc.arena.allocator(), '/'),
                    'b' => try buf.append(self.doc.arena.allocator(), 0x08),
                    'f' => try buf.append(self.doc.arena.allocator(), 0x0c),
                    'n' => try buf.append(self.doc.arena.allocator(), '\n'),
                    'r' => try buf.append(self.doc.arena.allocator(), '\r'),
                    't' => try buf.append(self.doc.arena.allocator(), '\t'),
                    'u' => {
                        if (self.idx + 4 >= self.text.len) return error.UnexpectedEnd;
                        const code = try self.parseHex4(self.text[self.idx + 1 .. self.idx + 5]);
                        var utf8: [4]u8 = undefined;
                        const len = std.unicode.utf8Encode(code, &utf8) catch return error.InvalidEscape;
                        try buf.appendSlice(self.doc.arena.allocator(), utf8[0..len]);
                        self.idx += 4;
                    },
                    else => return error.InvalidEscape,
                }
                self.idx += 1;
                continue;
            }
            try buf.append(self.doc.arena.allocator(), c);
            self.idx += 1;
        }
        return error.UnexpectedEnd;
    }

    fn parseHex4(self: *Parser, slice: []const u8) ParseError!u21 {
        _ = self;
        var code: u21 = 0;
        for (slice) |c| {
            code <<= 4;
            code |= switch (c) {
                '0'...'9' => @intCast(c - '0'),
                'a'...'f' => @intCast(c - 'a' + 10),
                'A'...'F' => @intCast(c - 'A' + 10),
                else => return error.InvalidEscape,
            };
        }
        return code;
    }

    fn parseNumber(self: *Parser) ParseError!NodeIndex {
        const start = self.idx;
        while (self.idx < self.text.len) {
            const c = self.text[self.idx];
            if ((c >= '0' and c <= '9') or c == '-' or c == '+' or c == '.' or c == 'e' or c == 'E') {
                self.idx += 1;
            } else break;
        }
        if (self.idx == start) return error.InvalidNumber;
        const end = self.idx;
        const slice = self.text[start..end];
        return self.addNode(.number, start, end, .{ .number = slice });
    }

    fn parseBool(self: *Parser) ParseError!NodeIndex {
        const start = self.idx;
        if (std.mem.startsWith(u8, self.text[self.idx..], "true")) {
            self.idx += 4;
            return self.addNode(.bool, start, self.idx, .{ .bool = true });
        }
        if (std.mem.startsWith(u8, self.text[self.idx..], "false")) {
            self.idx += 5;
            return self.addNode(.bool, start, self.idx, .{ .bool = false });
        }
        return error.InvalidBoolean;
    }

    fn parseNull(self: *Parser) ParseError!NodeIndex {
        const start = self.idx;
        if (!std.mem.startsWith(u8, self.text[self.idx..], "null")) return error.InvalidNull;
        self.idx += 4;
        return self.addNode(.null, start, self.idx, .{ .null = {} });
    }

    fn addNode(self: *Parser, kind: NodeKind, start: usize, end: usize, data: Node.Data) ParseError!NodeIndex {
        const idx: NodeIndex = @intCast(self.doc.nodes.items.len);
        try self.doc.nodes.append(self.doc.allocator, .{
            .kind = kind,
            .start = @intCast(start),
            .end = @intCast(end),
            .data = data,
        });
        return idx;
    }

    fn skipWhitespace(self: *Parser) void {
        while (self.idx < self.text.len) {
            const c = self.text[self.idx];
            if (c == ' ' or c == '\n' or c == '\r' or c == '\t') {
                self.idx += 1;
            } else break;
        }
    }

    fn peekChar(self: *Parser, c: u8) bool {
        return self.idx < self.text.len and self.text[self.idx] == c;
    }

    fn expectChar(self: *Parser, c: u8) ParseError!void {
        if (self.idx >= self.text.len or self.text[self.idx] != c) {
            return error.InvalidJson;
        }
        self.idx += 1;
    }
};
