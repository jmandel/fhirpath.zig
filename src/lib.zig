const std = @import("std");

pub const ast = @import("ast.zig");
pub const lexer = @import("lexer.zig");
pub const parser = @import("parser.zig");
pub const eval = @import("eval.zig");

pub fn parseExpression(allocator: std.mem.Allocator, input: []const u8) !ast.Expr {
    var p = parser.Parser.init(allocator, input);
    return p.parseExpression();
}

pub fn evalWithJson(
    allocator: std.mem.Allocator,
    expr: ast.Expr,
    json_text: []const u8,
    env: ?*eval.Env,
) !eval.EvalResult {
    return eval.evalWithJson(allocator, expr, json_text, env);
}

const testing = std.testing;

test "lexer basic identifiers" {
    const input = "name.given";
    var lex = lexer.Lexer.init(input);
    try testing.expectEqual(lexer.TokenKind.Identifier, (try lex.next()).kind);
    try testing.expectEqual(lexer.TokenKind.Dot, (try lex.next()).kind);
    try testing.expectEqual(lexer.TokenKind.Identifier, (try lex.next()).kind);
}

test "parse simple path" {
    const input = "name.given";
    const expr = try parseExpression(testing.allocator, input);
    defer ast.deinitExpr(testing.allocator, expr);
    switch (expr) {
        .Path => |p| {
            try testing.expectEqual(@as(usize, 2), p.steps.len);
            switch (p.steps[0]) {
                .Property => |name| try testing.expect(std.mem.eql(u8, name, "name")),
                else => try testing.expect(false),
            }
            switch (p.steps[1]) {
                .Property => |name| try testing.expect(std.mem.eql(u8, name, "given")),
                else => try testing.expect(false),
            }
        },
        else => try testing.expect(false),
    }
}
