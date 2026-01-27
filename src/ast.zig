const std = @import("std");

pub const Expr = union(enum) {
    Path: PathExpr,
    Binary: BinaryExpr,
    Literal: Literal,
};

pub const PathExpr = struct {
    root: Root,
    steps: []const Step,
};

pub const Root = union(enum) {
    This,
    Env: []const u8,
    Index,
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
    Eq,
};

pub const Literal = union(enum) {
    Null,
    Bool: bool,
    String: []const u8,
    Number: []const u8,
    Date: []const u8,
    DateTime: []const u8,
    Time: []const u8,
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
                .Date => |d| try writer.print("Date({s})", .{d}),
                .DateTime => |d| try writer.print("DateTime({s})", .{d}),
                .Time => |t| try writer.print("Time({s})", .{t}),
            }
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
                .This, .Index => {},
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
                .Date => |d| allocator.free(d),
                .DateTime => |d| allocator.free(d),
                .Time => |t| allocator.free(t),
                .Null, .Bool => {},
            }
        },
    }
}
