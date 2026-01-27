const std = @import("std");

pub const Expr = union(enum) {
    Path: PathExpr,
    Env: []const u8,
    Literal: Literal,
};

pub const PathExpr = struct {
    root: Root,
    segments: []const []const u8,
};

pub const Root = union(enum) {
    This,
    Env: []const u8,
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
            }
            for (p.segments) |seg| {
                try writer.writeAll(".");
                try writer.writeAll(seg);
            }
            try writer.writeAll(")");
        },
        .Env => |name| {
            try writer.writeAll("Env(%");
            try writer.writeAll(name);
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
                .This => {},
            }
            for (p.segments) |seg| {
                allocator.free(seg);
            }
            allocator.free(p.segments);
        },
        .Env => |name| allocator.free(name),
        .Literal => {}, // literals are currently input slices
    }
}
