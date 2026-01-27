const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");

// Parser is guided by the FHIRPath grammar in spec/fhirpath.g4.

pub const ParseError = error{
    UnexpectedToken,
    Unterminated,
    LexerError,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lex: lexer.Lexer,
    current: lexer.Token,

    pub fn init(allocator: std.mem.Allocator, input: []const u8) Parser {
        var lex = lexer.Lexer.init(input);
        const tok = lex.next() catch lexer.Token{ .kind = .Invalid, .lexeme = input[0..0], .start = 0, .end = 0 };
        return .{
            .allocator = allocator,
            .lex = lex,
            .current = tok,
        };
    }

    pub fn parseExpression(self: *Parser) !ast.Expr {
        const expr = try self.parsePathExpression();
        if (self.current.kind != .Eof) {
            return error.UnexpectedToken;
        }
        return expr;
    }

    fn parsePathExpression(self: *Parser) !ast.Expr {
        var root: ast.Root = .This;
        var segments = std.ArrayList([]const u8).empty;
        defer segments.deinit(self.allocator);

        if (self.current.kind == .Percent) {
            _ = try self.advance();
            const name = try self.parseEnvName();
            root = .{ .Env = name };
        } else {
            const name = try self.parseIdentifierName();
            try segments.append(self.allocator, name);
        }

        while (self.current.kind == .Dot) {
            _ = try self.advance();
            const name = try self.parseIdentifierName();
            try segments.append(self.allocator, name);
        }

        const segs = try self.allocator.dupe([]const u8, segments.items);
        return ast.Expr{ .Path = .{ .root = root, .segments = segs } };
    }

    fn parseIdentifierName(self: *Parser) ![]const u8 {
        switch (self.current.kind) {
            .Identifier => {
                const name = self.current.lexeme;
                _ = try self.advance();
                return try self.allocator.dupe(u8, name);
            },
            .DelimitedIdentifier => {
                const name = try self.unescapeDelimited(self.current.lexeme);
                _ = try self.advance();
                return name;
            },
            else => return error.UnexpectedToken,
        }
    }

    fn parseEnvName(self: *Parser) ![]const u8 {
        switch (self.current.kind) {
            .Identifier, .DelimitedIdentifier => return self.parseIdentifierName(),
            .String => {
                const name = try self.unescapeString(self.current.lexeme);
                _ = try self.advance();
                return name;
            },
            else => return error.UnexpectedToken,
        }
    }

    fn unescapeDelimited(self: *Parser, token: []const u8) ![]const u8 {
        if (token.len < 2 or token[0] != '`' or token[token.len - 1] != '`') {
            return error.UnexpectedToken;
        }
        const inner = token[1 .. token.len - 1];
        if (std.mem.indexOfScalar(u8, inner, '\\') == null) {
            return try self.allocator.dupe(u8, inner);
        }
        var out = std.ArrayList(u8).empty;
        defer out.deinit(self.allocator);
        var i: usize = 0;
        while (i < inner.len) : (i += 1) {
            if (inner[i] == '\\' and i + 1 < inner.len) {
                i += 1;
                try out.append(self.allocator, inner[i]);
                continue;
            }
            try out.append(self.allocator, inner[i]);
        }
        return out.toOwnedSlice(self.allocator);
    }

    fn unescapeString(self: *Parser, token: []const u8) ![]const u8 {
        if (token.len < 2 or token[0] != '\'' or token[token.len - 1] != '\'') {
            return error.UnexpectedToken;
        }
        const inner = token[1 .. token.len - 1];
        if (std.mem.indexOfScalar(u8, inner, '\\') == null) {
            return try self.allocator.dupe(u8, inner);
        }
        var out = std.ArrayList(u8).empty;
        defer out.deinit(self.allocator);
        var i: usize = 0;
        while (i < inner.len) : (i += 1) {
            if (inner[i] == '\\' and i + 1 < inner.len) {
                i += 1;
                try out.append(self.allocator, inner[i]);
                continue;
            }
            try out.append(self.allocator, inner[i]);
        }
        return out.toOwnedSlice(self.allocator);
    }

    fn advance(self: *Parser) !lexer.Token {
        const tok = self.lex.next() catch {
            return error.LexerError;
        };
        self.current = tok;
        return tok;
    }
};
