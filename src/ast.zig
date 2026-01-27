const std = @import("std");

pub const Expr = union(enum) {
    Path: PathExpr,
    Binary: BinaryExpr,
    Literal: Literal,
    TypeExpr: TypeExprNode, // for `is` and `as` operators
    Unary: UnaryExpr,       // for unary + and -
    Invoke: InvokeExpr,     // method call on arbitrary expression, e.g. (a | b).count()
};

pub const InvokeExpr = struct {
    operand: *Expr,
    steps: []const Step,
};

pub const TypeExprNode = struct {
    op: TypeOp,
    operand: *Expr,
    type_name: []const u8, // e.g., "String", "System.Integer", "FHIR.Patient"
};

pub const TypeOp = enum {
    Is,
    As,
};

pub const UnaryExpr = struct {
    op: UnaryOp,
    operand: *Expr,
};

pub const UnaryOp = enum {
    Plus,
    Minus,
};

pub const PathExpr = struct {
    root: Root,
    steps: []const Step,
};

pub const Root = union(enum) {
    This,
    Env: []const u8,
    Index,
    Total,
    Literal: Literal,
    Empty, // {} empty collection
};

pub const Step = union(enum) {
    Property: []const u8,
    Function: FunctionCall,
    Index: usize,
};

pub const FunctionCall = struct {
    name: []const u8,
    args: []const Expr,
};

pub const BinaryExpr = struct {
    op: BinaryOp,
    left: *Expr,
    right: *Expr,
};

pub const BinaryOp = enum {
    // Equality
    Eq,       // =
    NotEq,    // !=
    Equiv,    // ~
    NotEquiv, // !~

    // Comparison
    Lt,
    LtEq,
    Gt,
    GtEq,

    // Boolean
    And,
    Or,
    Xor,
    Implies,

    // Membership
    In,
    Contains,

    // Union (collection)
    Union,    // |

    // Arithmetic
    Add,      // +
    Sub,      // -
    Mul,      // *
    Div,      // /
    IntDiv,   // div
    Mod,      // mod

    // String concatenation
    Concat,   // &
};

pub const Literal = union(enum) {
    Null,
    Bool: bool,
    String: []const u8,
    Number: []const u8,
    Quantity: QuantityLiteral,
    Date: []const u8,
    DateTime: []const u8,
    Time: []const u8,
};

pub const QuantityLiteral = struct {
    value: []const u8,
    unit: []const u8,
};

pub fn formatExpr(expr: Expr, writer: anytype) !void {
    switch (expr) {
        .Path => |p| {
            try writer.writeAll("Path(");
            switch (p.root) {
                .This => try writer.writeAll("$this"),
                .Env => |name| {
                    try writer.writeAll("%");
                    try writer.writeAll(name);
                },
                .Index => try writer.writeAll("$index"),
                .Total => try writer.writeAll("$total"),
                .Literal => |lit| {
                    switch (lit) {
                        .Null => try writer.writeAll("null"),
                        .Bool => |b| try writer.print("{}", .{b}),
                        .String => |s| try writer.print("'{s}'", .{s}),
                        .Number => |n| try writer.print("{s}", .{n}),
                        .Quantity => |q| try writer.print("{s} '{s}'", .{ q.value, q.unit }),
                        .Date => |d| try writer.print("@{s}", .{d}),
                        .DateTime => |d| try writer.print("@{s}", .{d}),
                        .Time => |t| try writer.print("@T{s}", .{t}),
                    }
                },
                .Empty => try writer.writeAll("{}"),
            }
            for (p.steps) |step| {
                switch (step) {
                    .Property => |name| {
                        try writer.writeAll(".");
                        try writer.writeAll(name);
                    },
                    .Function => |call| {
                        try writer.writeAll(".");
                        try writer.writeAll(call.name);
                        try writer.writeAll("(");
                        for (call.args, 0..) |arg, idx| {
                            if (idx > 0) try writer.writeAll(", ");
                            try formatExpr(arg, writer);
                        }
                        try writer.writeAll(")");
                    },
                    .Index => |idx| {
                        try writer.print("[{d}]", .{idx});
                    },
                }
            }
            try writer.writeAll(")");
        },
        .Binary => |b| {
            try writer.writeAll("Binary(");
            try formatExpr(b.left.*, writer);
            try writer.writeAll(" ");
            switch (b.op) {
                .Eq => try writer.writeAll("="),
                .NotEq => try writer.writeAll("!="),
                .Equiv => try writer.writeAll("~"),
                .NotEquiv => try writer.writeAll("!~"),
                .Lt => try writer.writeAll("<"),
                .LtEq => try writer.writeAll("<="),
                .Gt => try writer.writeAll(">"),
                .GtEq => try writer.writeAll(">="),
                .And => try writer.writeAll("and"),
                .Or => try writer.writeAll("or"),
                .Xor => try writer.writeAll("xor"),
                .Implies => try writer.writeAll("implies"),
                .In => try writer.writeAll("in"),
                .Contains => try writer.writeAll("contains"),
                .Union => try writer.writeAll("|"),
                .Add => try writer.writeAll("+"),
                .Sub => try writer.writeAll("-"),
                .Mul => try writer.writeAll("*"),
                .Div => try writer.writeAll("/"),
                .IntDiv => try writer.writeAll("div"),
                .Mod => try writer.writeAll("mod"),
                .Concat => try writer.writeAll("&"),
            }
            try writer.writeAll(" ");
            try formatExpr(b.right.*, writer);
            try writer.writeAll(")");
        },
        .Literal => |lit| {
            switch (lit) {
                .Null => try writer.writeAll("Null"),
                .Bool => |b| try writer.print("Bool({})", .{b}),
                .String => |s| try writer.print("String('{s}')", .{s}),
                .Number => |n| try writer.print("Number({s})", .{n}),
                .Quantity => |q| try writer.print("Quantity({s} '{s}')", .{ q.value, q.unit }),
                .Date => |d| try writer.print("Date({s})", .{d}),
                .DateTime => |d| try writer.print("DateTime({s})", .{d}),
                .Time => |t| try writer.print("Time({s})", .{t}),
            }
        },
        .TypeExpr => |t| {
            try writer.writeAll("TypeExpr(");
            try formatExpr(t.operand.*, writer);
            switch (t.op) {
                .Is => try writer.writeAll(" is "),
                .As => try writer.writeAll(" as "),
            }
            try writer.writeAll(t.type_name);
            try writer.writeAll(")");
        },
        .Unary => |u| {
            try writer.writeAll("Unary(");
            switch (u.op) {
                .Plus => try writer.writeAll("+"),
                .Minus => try writer.writeAll("-"),
            }
            try formatExpr(u.operand.*, writer);
            try writer.writeAll(")");
        },
        .Invoke => |inv| {
            try writer.writeAll("Invoke(");
            try formatExpr(inv.operand.*, writer);
            for (inv.steps) |step| {
                switch (step) {
                    .Property => |name| {
                        try writer.writeAll(".");
                        try writer.writeAll(name);
                    },
                    .Function => |call| {
                        try writer.writeAll(".");
                        try writer.writeAll(call.name);
                        try writer.writeAll("(");
                        for (call.args, 0..) |arg, idx| {
                            if (idx > 0) try writer.writeAll(", ");
                            try formatExpr(arg, writer);
                        }
                        try writer.writeAll(")");
                    },
                    .Index => |idx| {
                        try writer.print("[{d}]", .{idx});
                    },
                }
            }
            try writer.writeAll(")");
        },
    }
}

pub fn formatExprAlloc(allocator: std.mem.Allocator, expr: Expr) ![]u8 {
    var list = std.ArrayList(u8).empty;
    defer list.deinit(allocator);
    try formatExpr(expr, list.writer(allocator));
    return list.toOwnedSlice(allocator);
}

pub fn deinitExpr(allocator: std.mem.Allocator, expr: Expr) void {
    switch (expr) {
        .Path => |p| {
            switch (p.root) {
                .Env => |name| allocator.free(name),
                .This, .Index, .Total, .Empty => {},
                .Literal => |lit| {
                    switch (lit) {
                        .String => |s| allocator.free(s),
                        .Number => |n| allocator.free(n),
                        .Quantity => |q| {
                            allocator.free(q.value);
                            allocator.free(q.unit);
                        },
                        .Date => |d| allocator.free(d),
                        .DateTime => |d| allocator.free(d),
                        .Time => |t| allocator.free(t),
                        .Null, .Bool => {},
                    }
                },
            }
            for (p.steps) |step| {
                switch (step) {
                    .Property => |name| allocator.free(name),
                    .Function => |call| {
                        allocator.free(call.name);
                        for (call.args) |arg| {
                            deinitExpr(allocator, arg);
                        }
                        allocator.free(call.args);
                    },
                    .Index => {},
                }
            }
            allocator.free(p.steps);
        },
        .Binary => |b| {
            deinitExpr(allocator, b.left.*);
            deinitExpr(allocator, b.right.*);
            allocator.destroy(b.left);
            allocator.destroy(b.right);
        },
        .Literal => |lit| {
            switch (lit) {
                .String => |s| allocator.free(s),
                .Number => |n| allocator.free(n),
                .Quantity => |q| {
                    allocator.free(q.value);
                    allocator.free(q.unit);
                },
                .Date => |d| allocator.free(d),
                .DateTime => |d| allocator.free(d),
                .Time => |t| allocator.free(t),
                .Null, .Bool => {},
            }
        },
        .TypeExpr => |t| {
            deinitExpr(allocator, t.operand.*);
            allocator.destroy(t.operand);
            allocator.free(t.type_name);
        },
        .Unary => |u| {
            deinitExpr(allocator, u.operand.*);
            allocator.destroy(u.operand);
        },
        .Invoke => |inv| {
            deinitExpr(allocator, inv.operand.*);
            allocator.destroy(inv.operand);
            for (inv.steps) |step| {
                switch (step) {
                    .Property => |name| allocator.free(name),
                    .Function => |call| {
                        allocator.free(call.name);
                        for (call.args) |arg| {
                            deinitExpr(allocator, arg);
                        }
                        allocator.free(call.args);
                    },
                    .Index => {},
                }
            }
            allocator.free(inv.steps);
        },
    }
}
