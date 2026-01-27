const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");

// Parser is guided by the FHIRPath grammar in spec/fhirpath.g4.
// Implements precedence-climbing for binary operators.

pub const ParseError = error{
    UnexpectedToken,
    Unterminated,
    LexerError,
};

// Precedence levels (lower number = lower precedence = binds looser)
const Precedence = enum(u8) {
    none = 0,
    implies = 1,     // implies
    or_xor = 2,      // or, xor
    and_ = 3,        // and
    membership = 4,  // in, contains
    equality = 5,    // =, ~, !=, !~
    comparison = 6,  // <, <=, >, >=
    type_ = 7,       // is, as (spec line 3132: binds tighter than comparison)
    union_ = 8,      // |
    additive = 9,    // +, -, &
    multiplicative = 10, // *, /, div, mod
    unary = 11,      // unary +, -
    primary = 12,    // . [] ()
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
        const expr = try self.parseExprPrec(.none);
        if (self.current.kind != .Eof) {
            return error.UnexpectedToken;
        }
        return expr;
    }

    const ParseErrorSet = ParseError || error{OutOfMemory};

    // Main precedence-climbing parser
    fn parseExprPrec(self: *Parser, min_prec: Precedence) ParseErrorSet!ast.Expr {
        var left = try self.parseUnaryOrPrimary();

        while (true) {
            const op_info = self.getBinaryOp();
            if (op_info == null) break;
            const info = op_info.?;
            if (@intFromEnum(info.prec) < @intFromEnum(min_prec)) break;

            // Consume the operator token(s)
            _ = try self.advance();

            // Handle `is` and `as` specially - they take a type specifier, not an expression
            if (info.type_op != null) {
                const type_name = try self.parseTypeSpecifier();
                const left_ptr = try self.allocator.create(ast.Expr);
                left_ptr.* = left;
                left = ast.Expr{ .TypeExpr = .{ .op = info.type_op.?, .operand = left_ptr, .type_name = type_name } };
                continue;
            }

            // For normal binary ops, parse right side with higher precedence for left-assoc
            const next_prec: Precedence = @enumFromInt(@intFromEnum(info.prec) + 1);
            const right = try self.parseExprPrec(next_prec);

            const left_ptr = try self.allocator.create(ast.Expr);
            left_ptr.* = left;
            const right_ptr = try self.allocator.create(ast.Expr);
            right_ptr.* = right;

            left = ast.Expr{ .Binary = .{ .op = info.op.?, .left = left_ptr, .right = right_ptr } };
        }

        return left;
    }

    const OpInfo = struct {
        op: ?ast.BinaryOp,
        prec: Precedence,
        type_op: ?ast.TypeOp = null,
    };

    fn getBinaryOp(self: *Parser) ?OpInfo {
        return switch (self.current.kind) {
            // Equality
            .Eq => .{ .op = .Eq, .prec = .equality },
            .BangEq => .{ .op = .NotEq, .prec = .equality },
            .Tilde => .{ .op = .Equiv, .prec = .equality },
            .BangTilde => .{ .op = .NotEquiv, .prec = .equality },

            // Comparison
            .Lt => .{ .op = .Lt, .prec = .comparison },
            .LtEq => .{ .op = .LtEq, .prec = .comparison },
            .Gt => .{ .op = .Gt, .prec = .comparison },
            .GtEq => .{ .op = .GtEq, .prec = .comparison },

            // Union
            .Pipe => .{ .op = .Union, .prec = .union_ },

            // Arithmetic
            .Plus => .{ .op = .Add, .prec = .additive },
            .Minus => .{ .op = .Sub, .prec = .additive },
            .Star => .{ .op = .Mul, .prec = .multiplicative },
            .Slash => .{ .op = .Div, .prec = .multiplicative },
            .Amp => .{ .op = .Concat, .prec = .additive },

            // Keywords as operators
            .Identifier => blk: {
                const lex = self.current.lexeme;
                if (std.mem.eql(u8, lex, "and")) break :blk OpInfo{ .op = .And, .prec = .and_ };
                if (std.mem.eql(u8, lex, "or")) break :blk OpInfo{ .op = .Or, .prec = .or_xor };
                if (std.mem.eql(u8, lex, "xor")) break :blk OpInfo{ .op = .Xor, .prec = .or_xor };
                if (std.mem.eql(u8, lex, "implies")) break :blk OpInfo{ .op = .Implies, .prec = .implies };
                if (std.mem.eql(u8, lex, "in")) break :blk OpInfo{ .op = .In, .prec = .membership };
                if (std.mem.eql(u8, lex, "contains")) break :blk OpInfo{ .op = .Contains, .prec = .membership };
                if (std.mem.eql(u8, lex, "div")) break :blk OpInfo{ .op = .IntDiv, .prec = .multiplicative };
                if (std.mem.eql(u8, lex, "mod")) break :blk OpInfo{ .op = .Mod, .prec = .multiplicative };
                if (std.mem.eql(u8, lex, "is")) break :blk OpInfo{ .op = null, .prec = .type_, .type_op = .Is };
                if (std.mem.eql(u8, lex, "as")) break :blk OpInfo{ .op = null, .prec = .type_, .type_op = .As };
                break :blk null;
            },
            else => null,
        };
    }

    fn parseTypeSpecifier(self: *Parser) ParseErrorSet![]const u8 {
        // Parse qualified identifier: identifier ('.' identifier)*
        var parts = std.ArrayList(u8).empty;
        defer parts.deinit(self.allocator);

        const first = try self.parseIdentifierName();
        try parts.appendSlice(self.allocator, first);
        self.allocator.free(first);

        while (self.current.kind == .Dot) {
            _ = try self.advance();
            const next = try self.parseIdentifierName();
            try parts.append(self.allocator, '.');
            try parts.appendSlice(self.allocator, next);
            self.allocator.free(next);
        }

        return parts.toOwnedSlice(self.allocator);
    }

    fn parseUnaryOrPrimary(self: *Parser) ParseErrorSet!ast.Expr {
        // Handle unary + and -
        if (self.current.kind == .Plus) {
            _ = try self.advance();
            const operand = try self.parseUnaryOrPrimary();
            const operand_ptr = try self.allocator.create(ast.Expr);
            operand_ptr.* = operand;
            return ast.Expr{ .Unary = .{ .op = .Plus, .operand = operand_ptr } };
        }
        if (self.current.kind == .Minus) {
            _ = try self.advance();
            const operand = try self.parseUnaryOrPrimary();
            const operand_ptr = try self.allocator.create(ast.Expr);
            operand_ptr.* = operand;
            return ast.Expr{ .Unary = .{ .op = .Minus, .operand = operand_ptr } };
        }
        return self.parsePrimary();
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
                if (self.current.kind == .String) {
                    const unit = try self.unescapeString(self.current.lexeme);
                    _ = try self.advance();
                    return self.parseTailSteps(.{ .Literal = .{ .Quantity = .{ .value = value, .unit = unit } } });
                }
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
                const expr = try self.parseExprPrec(.none);
                if (self.current.kind != .RParen) return error.UnexpectedToken;
                _ = try self.advance();
                // Support chaining on parenthesized expressions
                return self.parseTailStepsExpr(expr);
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
        if (steps.items.len == 0) {
            switch (root) {
                .Literal => |lit| return ast.Expr{ .Literal = lit },
                else => {},
            }
        }

        const step_slice = try self.allocator.dupe(ast.Step, steps.items);
        return ast.Expr{ .Path = .{ .root = root, .steps = step_slice } };
    }

    // Parse tail steps for an arbitrary expression (for parenthesized expressions)
    fn parseTailStepsExpr(self: *Parser, base: ast.Expr) ParseErrorSet!ast.Expr {
        // Check if there are any tail steps
        if (self.current.kind != .Dot and self.current.kind != .LBracket) {
            return base;
        }

        // Parse the steps
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

        const base_ptr = try self.allocator.create(ast.Expr);
        base_ptr.* = base;
        const step_slice = try self.allocator.dupe(ast.Step, steps.items);
        return ast.Expr{ .Invoke = .{ .operand = base_ptr, .steps = step_slice } };
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
            } else if (std.mem.eql(u8, name, "total")) {
                root = .Total;
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
                const expr = try self.parseExprPrec(.none);
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
