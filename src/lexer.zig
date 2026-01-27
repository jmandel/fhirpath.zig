const std = @import("std");

// Lexer follows the lexical rules in spec/fhirpath.g4 (N1).

pub const TokenKind = enum {
    Eof,
    Invalid,

    Identifier,
    DelimitedIdentifier,

    String,
    Number,
    Date,
    DateTime,
    Time,

    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Dot,
    Comma,
    Percent,
    Dollar,

    Plus,
    Minus,
    Star,
    Slash,
    Amp,
    Pipe,

    Eq,
    BangEq,
    Tilde,
    BangTilde,
    Lt,
    LtEq,
    Gt,
    GtEq,
};

pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
    start: usize,
    end: usize,
};

pub const LexerError = struct {
    pos: usize,
    message: []const u8,
};

pub const Lexer = struct {
    input: []const u8,
    index: usize,
    last_error: ?LexerError,

    pub fn init(input: []const u8) Lexer {
        return .{ .input = input, .index = 0, .last_error = null };
    }

    pub fn next(self: *Lexer) !Token {
        try self.skipTrivia();
        if (self.index >= self.input.len) {
            return self.token(.Eof, self.index, self.index);
        }

        const c = self.input[self.index];
        switch (c) {
            '(' => return self.single(.LParen),
            ')' => return self.single(.RParen),
            '[' => return self.single(.LBracket),
            ']' => return self.single(.RBracket),
            '{' => return self.single(.LBrace),
            '}' => return self.single(.RBrace),
            '.' => return self.single(.Dot),
            ',' => return self.single(.Comma),
            '%' => return self.single(.Percent),
            '$' => return self.single(.Dollar),
            '+' => return self.single(.Plus),
            '-' => return self.single(.Minus),
            '*' => return self.single(.Star),
            '/' => return self.single(.Slash),
            '&' => return self.single(.Amp),
            '|' => return self.single(.Pipe),
            '=' => return self.single(.Eq),
            '~' => return self.single(.Tilde),
            '!' => return self.lexBang(),
            '<' => return self.lexLt(),
            '>' => return self.lexGt(),
            '\'' => return self.lexString(),
            '`' => return self.lexDelimitedIdentifier(),
            '@' => return self.lexAtLiteral(),
            else => {},
        }

        if (isIdentStart(c)) {
            return self.lexIdentifier();
        }
        if (isDigit(c)) {
            return self.lexNumber();
        }

        return self.errorToken("unexpected character");
    }

    fn token(self: *Lexer, kind: TokenKind, start: usize, end: usize) Token {
        return .{ .kind = kind, .lexeme = self.input[start..end], .start = start, .end = end };
    }

    fn single(self: *Lexer, kind: TokenKind) Token {
        const start = self.index;
        self.index += 1;
        return self.token(kind, start, self.index);
    }

    fn errorToken(self: *Lexer, message: []const u8) Token {
        const pos = self.index;
        self.last_error = .{ .pos = pos, .message = message };
        return self.token(.Invalid, pos, pos + 1);
    }

    fn skipTrivia(self: *Lexer) !void {
        while (self.index < self.input.len) {
            const c = self.input[self.index];
            if (isWhitespace(c)) {
                self.index += 1;
                continue;
            }
            if (c == '/') {
                if (self.peek(1) == '/') {
                    self.index += 2;
                    while (self.index < self.input.len and self.input[self.index] != '\n') {
                        self.index += 1;
                    }
                    continue;
                }
                if (self.peek(1) == '*') {
                    self.index += 2;
                    while (self.index + 1 < self.input.len) {
                        if (self.input[self.index] == '*' and self.input[self.index + 1] == '/') {
                            self.index += 2;
                            break;
                        }
                        self.index += 1;
                    }
                    continue;
                }
            }
            break;
        }
    }

    fn lexIdentifier(self: *Lexer) Token {
        const start = self.index;
        self.index += 1;
        while (self.index < self.input.len) {
            const c = self.input[self.index];
            if (!isIdentContinue(c)) break;
            self.index += 1;
        }
        return self.token(.Identifier, start, self.index);
    }

    fn lexDelimitedIdentifier(self: *Lexer) Token {
        const start = self.index;
        self.index += 1; // skip backtick
        while (self.index < self.input.len) {
            const c = self.input[self.index];
            if (c == '`') {
                const end = self.index;
                self.index += 1; // consume backtick
                return self.token(.DelimitedIdentifier, start, end + 1);
            }
            if (c == '\\' and self.index + 1 < self.input.len) {
                self.index += 2;
                continue;
            }
            self.index += 1;
        }
        return self.errorToken("unterminated delimited identifier");
    }

    fn lexString(self: *Lexer) Token {
        const start = self.index;
        self.index += 1; // skip '
        while (self.index < self.input.len) {
            const c = self.input[self.index];
            if (c == '\\' and self.index + 1 < self.input.len) {
                self.index += 2;
                continue;
            }
            if (c == '\'') {
                const end = self.index;
                self.index += 1;
                return self.token(.String, start, end + 1);
            }
            self.index += 1;
        }
        return self.errorToken("unterminated string literal");
    }

    fn lexNumber(self: *Lexer) Token {
        const start = self.index;
        while (self.index < self.input.len and isDigit(self.input[self.index])) {
            self.index += 1;
        }
        if (self.index < self.input.len and self.input[self.index] == '.') {
            if (self.index + 1 < self.input.len and isDigit(self.input[self.index + 1])) {
                self.index += 1; // consume '.'
                while (self.index < self.input.len and isDigit(self.input[self.index])) {
                    self.index += 1;
                }
            }
        }
        return self.token(.Number, start, self.index);
    }

    fn lexAtLiteral(self: *Lexer) Token {
        const start = self.index;
        self.index += 1; // '@'
        if (self.index < self.input.len and self.input[self.index] == 'T') {
            // time literal
            self.index += 1;
            self.consumeDateTimeRun();
            return self.token(.Time, start, self.index);
        }
        self.consumeDateTimeRun();
        if (self.index < self.input.len and self.input[self.index] == 'T') {
            self.index += 1;
            self.consumeDateTimeRun();
            return self.token(.DateTime, start, self.index);
        }
        return self.token(.Date, start, self.index);
    }

    fn consumeDateTimeRun(self: *Lexer) void {
        while (self.index < self.input.len) {
            const c = self.input[self.index];
            if (c == '.') {
                if (self.index + 1 < self.input.len and isDigit(self.input[self.index + 1])) {
                    self.index += 1;
                    continue;
                }
                break;
            }
            if (isDateTimeChar(c)) {
                self.index += 1;
                continue;
            }
            break;
        }
    }

    fn lexBang(self: *Lexer) Token {
        const start = self.index;
        self.index += 1;
        if (self.index < self.input.len and self.input[self.index] == '=') {
            self.index += 1;
            return self.token(.BangEq, start, self.index);
        }
        if (self.index < self.input.len and self.input[self.index] == '~') {
            self.index += 1;
            return self.token(.BangTilde, start, self.index);
        }
        return self.token(.Invalid, start, self.index);
    }

    fn lexLt(self: *Lexer) Token {
        const start = self.index;
        self.index += 1;
        if (self.index < self.input.len and self.input[self.index] == '=') {
            self.index += 1;
            return self.token(.LtEq, start, self.index);
        }
        return self.token(.Lt, start, self.index);
    }

    fn lexGt(self: *Lexer) Token {
        const start = self.index;
        self.index += 1;
        if (self.index < self.input.len and self.input[self.index] == '=') {
            self.index += 1;
            return self.token(.GtEq, start, self.index);
        }
        return self.token(.Gt, start, self.index);
    }

    fn peek(self: *Lexer, offset: usize) u8 {
        const idx = self.index + offset;
        if (idx >= self.input.len) return 0;
        return self.input[idx];
    }
};

fn isWhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isIdentStart(c: u8) bool {
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z') or c == '_';
}

fn isIdentContinue(c: u8) bool {
    return isIdentStart(c) or isDigit(c);
}

fn isDateTimeChar(c: u8) bool {
    return isDigit(c) or c == '-' or c == ':' or c == 'Z' or c == '+';
}
