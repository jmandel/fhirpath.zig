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
        const expr = try self.parseEquality();
        if (self.current.kind != .Eof) {
            return error.UnexpectedToken;
        }
        return expr;
    }

    const ParseErrorSet = ParseError || error{OutOfMemory};

    fn parseEquality(self: *Parser) ParseErrorSet!ast.Expr {
        var left = try self.parsePrimary();
        while (self.current.kind == .Eq) {
            _ = try self.advance();
            const right = try self.parsePrimary();
            const left_ptr = try self.allocator.create(ast.Expr);
            left_ptr.* = left;
            const right_ptr = try self.allocator.create(ast.Expr);
            right_ptr.* = right;
            left = ast.Expr{ .Binary = .{ .op = .Eq, .left = left_ptr, .right = right_ptr } };
        }
        return left;
    }

    fn parsePrimary(self: *Parser) ParseErrorSet!ast.Expr {
        switch (self.current.kind) {
            .String => {
                const value = try self.unescapeString(self.current.lexeme);
                _ = try self.advance();
                return self.parseTailSteps(.{ .Literal = .{ .String = value } });
            },
            .Number => {
                const value = try self.allocator.dupe(u8, self.current.lexeme);
                _ = try self.advance();
                return self.parseTailSteps(.{ .Literal = .{ .Number = value } });
            },
            .Date => {
                const value = try self.allocator.dupe(u8, self.current.lexeme);
                _ = try self.advance();
                return self.parseTailSteps(.{ .Literal = .{ .Date = value } });
            },
            .DateTime => {
                const value = try self.allocator.dupe(u8, self.current.lexeme);
                _ = try self.advance();
                return self.parseTailSteps(.{ .Literal = .{ .DateTime = value } });
            },
            .Time => {
                const value = try self.allocator.dupe(u8, self.current.lexeme);
                _ = try self.advance();
                return self.parseTailSteps(.{ .Literal = .{ .Time = value } });
            },
            .Identifier => {
                const lex = self.current.lexeme;
                if (std.mem.eql(u8, lex, "true")) {
                    _ = try self.advance();
                    return self.parseTailSteps(.{ .Literal = .{ .Bool = true } });
                }
                if (std.mem.eql(u8, lex, "false")) {
                    _ = try self.advance();
                    return self.parseTailSteps(.{ .Literal = .{ .Bool = false } });
                }
                if (std.mem.eql(u8, lex, "null")) {
                    _ = try self.advance();
                    return self.parseTailSteps(.{ .Literal = .{ .Null = {} } });
                }
                return self.parsePathExpression();
            },
            .DelimitedIdentifier, .Percent, .Dollar => return self.parsePathExpression(),
            .LBrace => {
                _ = try self.advance();
                if (self.current.kind != .RBrace) return error.UnexpectedToken;
                _ = try self.advance();
                return self.parseTailSteps(.Empty);
            },
            .LParen => {
                _ = try self.advance();
                const expr = try self.parseEquality();
                if (self.current.kind != .RParen) return error.UnexpectedToken;
                _ = try self.advance();
                // TODO: support chaining on parenthesized expressions
                return expr;
            },
            .Minus => {
                // Unary minus: - expression
                _ = try self.advance();
                // For now, only support negative number literals
                if (self.current.kind != .Number) return error.UnexpectedToken;
                const num = self.current.lexeme;
                _ = try self.advance();
                // Allocate string with leading minus
                const neg_num = try std.fmt.allocPrint(self.allocator, "-{s}", .{num});
                return self.parseTailSteps(.{ .Literal = .{ .Number = neg_num } });
            },
            .Plus => {
                // Unary plus: + expression (just consume the +)
                _ = try self.advance();
                return self.parsePrimary();
            },
            else => return error.UnexpectedToken,
        }
    }

    fn parseTailSteps(self: *Parser, root: ast.Root) ParseErrorSet!ast.Expr {
        var steps = std.ArrayList(ast.Step).empty;
        defer steps.deinit(self.allocator);

        while (true) {
            switch (self.current.kind) {
                .Dot => {
                    _ = try self.advance();
                    const name = try self.parseIdentifierName();
                    if (self.current.kind == .LParen) {
                        const call = try self.parseFunctionCall(name);
                        try steps.append(self.allocator, .{ .Function = call });
                    } else {
                        try steps.append(self.allocator, .{ .Property = name });
                    }
                },
                .LBracket => {
                    _ = try self.advance();
                    if (self.current.kind != .Number) return error.UnexpectedToken;
                    const idx = std.fmt.parseInt(usize, self.current.lexeme, 10) catch {
                        return error.UnexpectedToken;
                    };
                    _ = try self.advance();
                    if (self.current.kind != .RBracket) return error.UnexpectedToken;
                    _ = try self.advance();
                    try steps.append(self.allocator, .{ .Index = idx });
                },
                else => break,
            }
        }

        // If there are no steps and root is a literal, return Literal directly for simpler AST
        // Note: .Empty is kept as Path with root=.Empty to represent empty collection
        if (steps.items.len == 0) {
            switch (root) {
                .Literal => |lit| return ast.Expr{ .Literal = lit },
                else => {},
            }
        }

        const step_slice = try self.allocator.dupe(ast.Step, steps.items);
        return ast.Expr{ .Path = .{ .root = root, .steps = step_slice } };
    }

    fn parsePathExpression(self: *Parser) ParseErrorSet!ast.Expr {
        var root: ast.Root = .This;
        var steps = std.ArrayList(ast.Step).empty;
        defer steps.deinit(self.allocator);

        if (self.current.kind == .Percent) {
            _ = try self.advance();
            const name = try self.parseEnvName();
            root = .{ .Env = name };
        } else if (self.current.kind == .Dollar) {
            _ = try self.advance();
            if (self.current.kind != .Identifier) return error.UnexpectedToken;
            const name = self.current.lexeme;
            _ = try self.advance();
            if (std.mem.eql(u8, name, "this")) {
                root = .This;
            } else if (std.mem.eql(u8, name, "index")) {
                root = .Index;
            } else {
                return error.UnexpectedToken;
            }
        } else {
            const name = try self.parseIdentifierName();
            // Check if this is a standalone function call (identifier followed by '(')
            if (self.current.kind == .LParen) {
                const call = try self.parseFunctionCall(name);
                try steps.append(self.allocator, .{ .Function = call });
            } else {
                try steps.append(self.allocator, .{ .Property = name });
            }
        }

        while (true) {
            switch (self.current.kind) {
                .Dot => {
                    _ = try self.advance();
                    const name = try self.parseIdentifierName();
                    if (self.current.kind == .LParen) {
                        const call = try self.parseFunctionCall(name);
                        try steps.append(self.allocator, .{ .Function = call });
                    } else {
                        try steps.append(self.allocator, .{ .Property = name });
                    }
                },
                .LBracket => {
                    _ = try self.advance();
                    if (self.current.kind != .Number) return error.UnexpectedToken;
                    const idx = std.fmt.parseInt(usize, self.current.lexeme, 10) catch {
                        return error.UnexpectedToken;
                    };
                    _ = try self.advance();
                    if (self.current.kind != .RBracket) return error.UnexpectedToken;
                    _ = try self.advance();
                    try steps.append(self.allocator, .{ .Index = idx });
                },
                else => break,
            }
        }

        const step_slice = try self.allocator.dupe(ast.Step, steps.items);
        return ast.Expr{ .Path = .{ .root = root, .steps = step_slice } };
    }

    fn parseFunctionCall(self: *Parser, name: []const u8) ParseErrorSet!ast.FunctionCall {
        if (self.current.kind != .LParen) return error.UnexpectedToken;
        _ = try self.advance();
        var args = std.ArrayList(ast.Expr).empty;
        defer args.deinit(self.allocator);
        if (self.current.kind != .RParen) {
            while (true) {
                const expr = try self.parseEquality();
                try args.append(self.allocator, expr);
                if (self.current.kind == .Comma) {
                    _ = try self.advance();
                    continue;
                }
                break;
            }
        }
        if (self.current.kind != .RParen) return error.UnexpectedToken;
        _ = try self.advance();
        const arg_slice = try self.allocator.dupe(ast.Expr, args.items);
        return .{ .name = name, .args = arg_slice };
    }

    fn parseIdentifierName(self: *Parser) ParseErrorSet![]const u8 {
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

    fn parseEnvName(self: *Parser) ParseErrorSet![]const u8 {
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

    fn unescapeDelimited(self: *Parser, token: []const u8) ParseErrorSet![]const u8 {
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

    fn unescapeString(self: *Parser, token: []const u8) ParseErrorSet![]const u8 {
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

    fn advance(self: *Parser) ParseErrorSet!lexer.Token {
        const tok = self.lex.next() catch {
            return error.LexerError;
        };
        self.current = tok;
        return tok;
    }
};
