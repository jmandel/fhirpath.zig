//! Build a compact schema model blob from FHIR StructureDefinitions.
//! Reads profiles-types.json and profiles-resources.json, outputs model.bin.

const std = @import("std");

const SystemTypes = [_][]const u8{
    "System.Any",
    "System.Boolean",
    "System.Integer",
    "System.Decimal",
    "System.String",
    "System.Date",
    "System.DateTime",
    "System.Time",
    "System.Quantity",
};

const KIND_PRIMITIVE: u8 = 1;
const KIND_COMPLEX: u8 = 2;
const KIND_RESOURCE: u8 = 3;
const KIND_BACKBONE: u8 = 4;

const FIELD_MULTIPLE: u32 = 1 << 0;
const FIELD_CHOICE_BASE: u32 = 1 << 1;
const FIELD_CHOICE_VARIANT: u32 = 1 << 2;

const FieldInfo = struct {
    name: []const u8,
    child_type: ?[]const u8,
    child_system_id: ?u32,
    flags: u32,
    choice_group: u32,
};

const TypeInfo = struct {
    name: []const u8,
    kind: u8,
    base_name: ?[]const u8,
    prim_base_name: ?[]const u8,
    fields: std.ArrayListUnmanaged(FieldInfo) = .{},
};

const ChoiceVariant = struct {
    name: []const u8,
    type_name: ?[]const u8,
    system_id: ?u32,
};

const ChoiceGroup = struct {
    base_name: []const u8,
    variants: std.ArrayListUnmanaged(ChoiceVariant) = .{},
};

const TypeEntryOut = struct {
    name: []const u8,
    kind: u8,
    flags: u8,
    base_id: u32,
    prim_base_id: u32,
    field_start: u32,
    field_count: u32,
};

const FieldEntryOut = struct {
    name: []const u8,
    child_id: u32,
    flags: u32,
    choice_group: u32,
};

const NameIndexOut = struct {
    name: []const u8,
    type_index: u32,
};

const ChoiceGroupOut = struct {
    base_name: []const u8,
    variant_start: u32,
    variant_count: u32,
};

const ChoiceVariantOut = struct {
    name: []const u8,
    type_id: u32,
};

const StringSpan = struct { off: u32, len: u32 };

const StringPool = struct {
    allocator: std.mem.Allocator,
    data: std.ArrayListUnmanaged(u8) = .{},
    offsets: std.StringHashMapUnmanaged(StringSpan) = .{},

    fn init(allocator: std.mem.Allocator) StringPool {
        return .{ .allocator = allocator };
    }

    fn deinit(self: *StringPool) void {
        self.data.deinit(self.allocator);
        self.offsets.deinit(self.allocator);
    }

    fn intern(self: *StringPool, text: []const u8) !StringSpan {
        if (self.offsets.get(text)) |entry| return entry;
        const off: u32 = @intCast(self.data.items.len);
        const len: u32 = @intCast(text.len);
        try self.data.appendSlice(self.allocator, text);
        const key = try self.allocator.dupe(u8, text);
        try self.offsets.put(self.allocator, key, .{ .off = off, .len = len });
        return .{ .off = off, .len = len };
    }
};

const Builder = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayListUnmanaged(TypeInfo) = .{},
    type_map: std.StringHashMapUnmanaged(u32) = .{},
    choice_groups: std.ArrayListUnmanaged(ChoiceGroup) = .{},
    choice_group_map: std.StringHashMapUnmanaged(u32) = .{},

    fn deinit(self: *Builder) void {
        self.types.deinit(self.allocator);
        self.type_map.deinit(self.allocator);
        self.choice_groups.deinit(self.allocator);
        self.choice_group_map.deinit(self.allocator);
    }

    fn getTypeIndex(self: *Builder, name: []const u8) ?u32 {
        return self.type_map.get(name);
    }

    fn ensureType(self: *Builder, name: []const u8, kind: u8, base_name: ?[]const u8, prim_base: ?[]const u8) !u32 {
        if (self.type_map.get(name)) |idx| return idx;
        const idx: u32 = @intCast(self.types.items.len);
        try self.types.append(self.allocator, .{
            .name = name,
            .kind = kind,
            .base_name = base_name,
            .prim_base_name = prim_base,
        });
        try self.type_map.put(self.allocator, name, idx);
        return idx;
    }

    fn addField(
        self: *Builder,
        type_idx: u32,
        name: []const u8,
        child_type: ?[]const u8,
        child_system_id: ?u32,
        flags: u32,
        choice_group: u32,
    ) !void {
        const t = &self.types.items[type_idx];
        for (t.fields.items) |*field| {
            if (std.mem.eql(u8, field.name, name)) {
                field.flags |= flags;
                if (field.child_type == null and child_type != null) field.child_type = child_type;
                if (field.child_system_id == null and child_system_id != null) field.child_system_id = child_system_id;
                if (choice_group != 0) field.choice_group = choice_group;
                return;
            }
        }
        try t.fields.append(self.allocator, .{
            .name = name,
            .child_type = child_type,
            .child_system_id = child_system_id,
            .flags = flags,
            .choice_group = choice_group,
        });
    }

    fn getChoiceGroup(self: *Builder, key: []const u8, base_name: []const u8) !u32 {
        if (self.choice_group_map.get(key)) |idx| return idx;
        const idx: u32 = @intCast(self.choice_groups.items.len);
        try self.choice_groups.append(self.allocator, .{ .base_name = base_name });
        try self.choice_group_map.put(self.allocator, key, idx);
        return idx;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var version: []const u8 = "r5";
    var in_dir: ?[]const u8 = null;
    var out_path: ?[]const u8 = null;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--fhir-version") and i + 1 < args.len) {
            version = args[i + 1];
            i += 1;
        } else if (std.mem.eql(u8, arg, "--in-dir") and i + 1 < args.len) {
            in_dir = args[i + 1];
            i += 1;
        } else if (std.mem.eql(u8, arg, "--out") and i + 1 < args.len) {
            out_path = args[i + 1];
            i += 1;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        }
    }

    const root = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(root);
    const version_lower = try toLower(allocator, version);
    defer allocator.free(version_lower);

    const base_dir = if (in_dir) |dir|
        dir
    else
        try std.fmt.allocPrint(allocator, "{s}/models/{s}", .{ root, version_lower });

    const output_path = if (out_path) |out|
        out
    else
        try std.fmt.allocPrint(allocator, "{s}/model.bin", .{ base_dir });

    const types_path = try std.fmt.allocPrint(allocator, "{s}/profiles-types.json", .{ base_dir });
    const resources_path = try std.fmt.allocPrint(allocator, "{s}/profiles-resources.json", .{ base_dir });

    var defs = std.ArrayListUnmanaged(std.json.Value){};
    defer defs.deinit(allocator);
    try collectDefs(allocator, types_path, &defs);
    try collectDefs(allocator, resources_path, &defs);

    var builder = Builder{ .allocator = allocator };
    defer builder.deinit();

    try buildTypes(&builder, defs.items);
    try buildBackboneTypes(&builder, defs.items);
    try buildFields(&builder, defs.items);

    var type_entries = std.ArrayListUnmanaged(TypeEntryOut){};
    var field_entries = std.ArrayListUnmanaged(FieldEntryOut){};
    var name_index = std.ArrayListUnmanaged(NameIndexOut){};
    var group_entries = std.ArrayListUnmanaged(ChoiceGroupOut){};
    var variant_entries = std.ArrayListUnmanaged(ChoiceVariantOut){};

    try emitTables(allocator, &builder, &type_entries, &field_entries, &name_index, &group_entries, &variant_entries);

    var pool = StringPool.init(allocator);
    defer pool.deinit();
    try collectStrings(&pool, type_entries.items, field_entries.items, name_index.items, group_entries.items, variant_entries.items);

    const output_bytes = try buildBlob(allocator, &pool, type_entries.items, field_entries.items, name_index.items, group_entries.items, variant_entries.items);
    defer allocator.free(output_bytes);

    try writeFile(output_path, output_bytes);
    std.debug.print("Wrote {s} ({d} bytes)\n", .{ output_path, output_bytes.len });
}

fn collectDefs(
    allocator: std.mem.Allocator,
    path: []const u8,
    defs: *std.ArrayListUnmanaged(std.json.Value),
) !void {
    const data = try std.fs.cwd().readFileAlloc(allocator, path, 200 * 1024 * 1024);
    defer allocator.free(data);
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, data, .{});
    defer parsed.deinit();
    const root = parsed.value;
    if (root != .object) return error.InvalidFormat;
    const entries_val = root.object.get("entry") orelse return;
    if (entries_val != .array) return;
    for (entries_val.array.items) |entry| {
        if (entry != .object) continue;
        const resource = entry.object.get("resource") orelse continue;
        if (resource != .object) continue;
        const rt = resource.object.get("resourceType") orelse continue;
        if (rt != .string) continue;
        if (!std.mem.eql(u8, rt.string, "StructureDefinition")) continue;
        try defs.append(allocator, resource);
    }
}

fn buildTypes(builder: *Builder, defs: []const std.json.Value) !void {
    for (defs) |sd| {
        const name = getString(sd, "type") orelse continue;
        if (builder.getTypeIndex(name) != null) continue;
        const kind = getString(sd, "kind") orelse "complex-type";
        const kind_id: u8 = if (std.mem.eql(u8, kind, "primitive-type"))
            KIND_PRIMITIVE
        else if (std.mem.eql(u8, kind, "resource"))
            KIND_RESOURCE
        else
            KIND_COMPLEX;
        const base_name = lastPathSegment(getString(sd, "baseDefinition"));
        const prim_base = if (kind_id == KIND_PRIMITIVE) base_name else null;
        _ = try builder.ensureType(name, kind_id, base_name, prim_base);
    }
}

fn buildBackboneTypes(builder: *Builder, defs: []const std.json.Value) !void {
    const backbone_base = if (builder.getTypeIndex("BackboneElement") != null) "BackboneElement" else "Element";
    for (defs) |sd| {
        const root = getString(sd, "type") orelse continue;
        const snapshot = getObject(sd, "snapshot") orelse continue;
        const elements = getArray(snapshot, "element") orelse continue;
        for (elements) |el| {
            const path = getString(el, "path") orelse continue;
            if (!std.mem.containsAtLeast(u8, path, 1, ".")) continue;
            const parent = path[0 .. std.mem.lastIndexOfScalar(u8, path, '.').?];
            if (std.mem.eql(u8, parent, root)) continue;
            _ = builder.ensureType(parent, KIND_BACKBONE, backbone_base, null) catch {};
        }
    }
}

fn buildFields(builder: *Builder, defs: []const std.json.Value) !void {
    for (defs) |sd| {
        const snapshot = getObject(sd, "snapshot") orelse continue;
        const elements = getArray(snapshot, "element") orelse continue;
        for (elements) |el| {
            const path = getString(el, "path") orelse continue;
            if (!std.mem.containsAtLeast(u8, path, 1, ".")) continue;
            const split_idx = std.mem.lastIndexOfScalar(u8, path, '.').?;
            const parent_name = path[0..split_idx];
            const field_name = path[split_idx + 1 ..];
            const parent_idx = builder.getTypeIndex(parent_name) orelse continue;

            const max_val = getString(el, "max") orelse "1";
            const multiple = !(std.mem.eql(u8, max_val, "0") or std.mem.eql(u8, max_val, "1"));
            const flags_base: u32 = if (multiple) FIELD_MULTIPLE else 0;

            if (std.mem.endsWith(u8, field_name, "[x]")) {
                const base_name = field_name[0 .. field_name.len - 3];
                const key = try std.fmt.allocPrint(builder.allocator, "{s}::{s}", .{ parent_name, base_name });
                const group_idx = try builder.getChoiceGroup(key, base_name);
                try builder.addField(parent_idx, base_name, null, null, flags_base | FIELD_CHOICE_BASE, group_idx + 1);

                const types_list = getArray(el, "type") orelse &[_]std.json.Value{};
                for (types_list) |type_entry| {
                    const code = getString(type_entry, "code") orelse continue;
                    const norm = normalizeTypeCode(code);
                    if (norm.local == null and norm.sys_id == null) continue;
                    const suffix = try capitalizeSuffix(builder.allocator, norm);
                    const variant_name = try std.fmt.allocPrint(builder.allocator, "{s}{s}", .{ base_name, suffix });
                    try builder.addField(parent_idx, variant_name, norm.local, norm.sys_id, flags_base | FIELD_CHOICE_VARIANT, group_idx + 1);
                    const group = &builder.choice_groups.items[group_idx];
                    try group.variants.append(builder.allocator, .{ .name = variant_name, .type_name = norm.local, .system_id = norm.sys_id });
                }
                continue;
            }

            var child_type: ?[]const u8 = null;
            var child_sys: ?u32 = null;
            if (getArray(el, "type")) |types_list| {
                if (types_list.len > 0) {
                    const type_entry = types_list[0];
                    const code = getString(type_entry, "code");
                    if (code) |c| {
                        const norm = normalizeTypeCode(c);
                        // If the code is a System URL (e.g. http://hl7.org/fhirpath/System.String),
                        // check for a structuredefinition-fhir-type extension that specifies the
                        // actual FHIR type (e.g. "id"). Use the FHIR type instead so that
                        // Resource.id is FHIR.id, not System.String.
                        if (norm.local == null and norm.sys_id != null) {
                            if (getFhirTypeExtension(type_entry)) |fhir_type| {
                                child_type = fhir_type;
                                child_sys = null;
                            } else {
                                child_type = norm.local;
                                child_sys = norm.sys_id;
                            }
                        } else {
                            child_type = norm.local;
                            child_sys = norm.sys_id;
                        }
                    }
                }
            } else if (getString(el, "contentReference")) |content_ref| {
                if (std.mem.startsWith(u8, content_ref, "#")) {
                    child_type = content_ref[1..];
                } else {
                    child_type = content_ref;
                }
            }

            if (child_type != null and (std.mem.eql(u8, child_type.?, "BackboneElement") or std.mem.eql(u8, child_type.?, "Element"))) {
                child_type = path;
            }

            try builder.addField(parent_idx, field_name, child_type, child_sys, flags_base, 0);
        }
    }
}

fn emitTables(
    allocator: std.mem.Allocator,
    builder: *Builder,
    type_entries: *std.ArrayListUnmanaged(TypeEntryOut),
    field_entries: *std.ArrayListUnmanaged(FieldEntryOut),
    name_index: *std.ArrayListUnmanaged(NameIndexOut),
    group_entries: *std.ArrayListUnmanaged(ChoiceGroupOut),
    variant_entries: *std.ArrayListUnmanaged(ChoiceVariantOut),
) !void {
    const type_count = builder.types.items.len;
    const type_indices = try allocator.alloc(u32, type_count);
    defer allocator.free(type_indices);
    for (type_indices, 0..) |*idx, i| idx.* = @intCast(i);
    std.mem.sort(u32, type_indices, builder.types.items, struct {
        fn lessThan(ctx: []TypeInfo, a: u32, b: u32) bool {
            return std.mem.lessThan(u8, ctx[a].name, ctx[b].name);
        }
    }.lessThan);

    var type_index_map = std.StringHashMapUnmanaged(u32){};
    defer type_index_map.deinit(allocator);
    for (type_indices, 0..) |idx, sorted_idx| {
        try type_index_map.put(allocator, builder.types.items[idx].name, @intCast(sorted_idx));
    }

    for (type_indices, 0..) |idx, sorted_idx| {
        const t = &builder.types.items[idx];
        std.mem.sort(FieldInfo, t.fields.items, {}, struct {
            fn lessThan(_: void, a: FieldInfo, b: FieldInfo) bool {
                return std.mem.lessThan(u8, a.name, b.name);
            }
        }.lessThan);

        const field_start: u32 = @intCast(field_entries.items.len);
        for (t.fields.items) |field| {
            var child_id: u32 = 0;
            if (field.child_system_id) |sid| {
                child_id = sid;
            } else if (field.child_type) |child_name| {
                if (type_index_map.get(child_name)) |child_idx| {
                    child_id = 0x8000_0000 | (child_idx + 1);
                } else {
                    continue;
                }
            }
            try field_entries.append(allocator, .{
                .name = field.name,
                .child_id = child_id,
                .flags = field.flags,
                .choice_group = field.choice_group,
            });
        }
        const field_count: u32 = @intCast(field_entries.items.len - field_start);
        var base_id: u32 = 0;
        if (t.base_name) |base_name| {
            if (type_index_map.get(base_name)) |base_idx| {
                base_id = 0x8000_0000 | (base_idx + 1);
            }
        }
        var prim_base_id: u32 = 0;
        if (t.prim_base_name) |prim| {
            if (type_index_map.get(prim)) |prim_idx| {
                prim_base_id = 0x8000_0000 | (prim_idx + 1);
            }
        }
        try type_entries.append(allocator, .{
            .name = t.name,
            .kind = t.kind,
            .flags = 0,
            .base_id = base_id,
            .prim_base_id = prim_base_id,
            .field_start = field_start,
            .field_count = field_count,
        });
        try name_index.append(allocator, .{ .name = t.name, .type_index = @intCast(sorted_idx) });
    }

    std.mem.sort(NameIndexOut, name_index.items, {}, struct {
        fn lessThan(_: void, a: NameIndexOut, b: NameIndexOut) bool {
            return std.mem.lessThan(u8, a.name, b.name);
        }
    }.lessThan);

    for (builder.choice_groups.items) |group| {
        const variant_start: u32 = @intCast(variant_entries.items.len);
        for (group.variants.items) |variant| {
            var type_id: u32 = 0;
            if (variant.system_id) |sid| {
                type_id = sid;
            } else if (variant.type_name) |name| {
                if (type_index_map.get(name)) |idx| {
                    type_id = 0x8000_0000 | (idx + 1);
                }
            }
            try variant_entries.append(allocator, .{ .name = variant.name, .type_id = type_id });
        }
        const variant_count: u32 = @intCast(variant_entries.items.len - variant_start);
        try group_entries.append(allocator, .{
            .base_name = group.base_name,
            .variant_start = variant_start,
            .variant_count = variant_count,
        });
    }
}

fn collectStrings(
    pool: *StringPool,
    type_entries: []const TypeEntryOut,
    field_entries: []const FieldEntryOut,
    name_index: []const NameIndexOut,
    group_entries: []const ChoiceGroupOut,
    variant_entries: []const ChoiceVariantOut,
) !void {
    for (type_entries) |t| _ = try pool.intern(t.name);
    for (field_entries) |f| _ = try pool.intern(f.name);
    for (name_index) |n| _ = try pool.intern(n.name);
    for (group_entries) |g| _ = try pool.intern(g.base_name);
    for (variant_entries) |v| _ = try pool.intern(v.name);
}

fn buildBlob(
    allocator: std.mem.Allocator,
    pool: *StringPool,
    type_entries: []const TypeEntryOut,
    field_entries: []const FieldEntryOut,
    name_index: []const NameIndexOut,
    group_entries: []const ChoiceGroupOut,
    variant_entries: []const ChoiceVariantOut,
) ![]u8 {
    const strings_off: u32 = 64;
    const strings_len: u32 = @intCast(pool.data.items.len);
    const types_off = align4(strings_off + strings_len);
    const name_index_off = align4(types_off + @as(u32, @intCast(type_entries.len)) * 24);
    const fields_off = align4(name_index_off + @as(u32, @intCast(name_index.len)) * 12);
    const groups_off = align4(fields_off + @as(u32, @intCast(field_entries.len)) * 20);
    const variants_off = align4(groups_off + @as(u32, @intCast(group_entries.len)) * 16);
    const total_size = align4(variants_off + @as(u32, @intCast(variant_entries.len)) * 12);

    var out = try allocator.alloc(u8, total_size);
    @memset(out, 0);
    @memcpy(out[0..4], "FPM4");
    writeU16(out, 4, 1);
    writeU16(out, 6, 64);

    writeU32(out, 8, total_size);
    writeU32(out, 12, strings_off);
    writeU32(out, 16, strings_len);
    writeU32(out, 20, types_off);
    writeU32(out, 24, @intCast(type_entries.len));
    writeU32(out, 28, name_index_off);
    writeU32(out, 32, @intCast(name_index.len));
    writeU32(out, 36, fields_off);
    writeU32(out, 40, @intCast(field_entries.len));
    writeU32(out, 44, groups_off);
    writeU32(out, 48, @intCast(group_entries.len));
    writeU32(out, 52, variants_off);
    writeU32(out, 56, @intCast(variant_entries.len));
    writeU32(out, 60, 0);

    @memcpy(out[strings_off .. strings_off + strings_len], pool.data.items);

    for (type_entries, 0..) |t, i| {
        const off = types_off + @as(u32, @intCast(i)) * 24;
        const name = try pool.intern(t.name);
        writeU32(out, off, name.off);
        writeU16(out, off + 4, @intCast(name.len));
        out[off + 6] = t.kind;
        out[off + 7] = t.flags;
        writeU32(out, off + 8, t.base_id);
        writeU32(out, off + 12, t.prim_base_id);
        writeU32(out, off + 16, t.field_start);
        writeU32(out, off + 20, t.field_count);
    }

    for (name_index, 0..) |n, i| {
        const off = name_index_off + @as(u32, @intCast(i)) * 12;
        const name = try pool.intern(n.name);
        writeU32(out, off, name.off);
        writeU32(out, off + 4, name.len);
        writeU32(out, off + 8, n.type_index);
    }

    for (field_entries, 0..) |f, i| {
        const off = fields_off + @as(u32, @intCast(i)) * 20;
        const name = try pool.intern(f.name);
        writeU32(out, off, name.off);
        writeU32(out, off + 4, name.len);
        writeU32(out, off + 8, f.child_id);
        writeU32(out, off + 12, f.flags);
        writeU32(out, off + 16, f.choice_group);
    }

    for (group_entries, 0..) |g, i| {
        const off = groups_off + @as(u32, @intCast(i)) * 16;
        const name = try pool.intern(g.base_name);
        writeU32(out, off, name.off);
        writeU32(out, off + 4, name.len);
        writeU32(out, off + 8, g.variant_start);
        writeU32(out, off + 12, g.variant_count);
    }

    for (variant_entries, 0..) |v, i| {
        const off = variants_off + @as(u32, @intCast(i)) * 12;
        const name = try pool.intern(v.name);
        writeU32(out, off, name.off);
        writeU32(out, off + 4, name.len);
        writeU32(out, off + 8, v.type_id);
    }

    return out;
}

fn writeFile(path: []const u8, data: []const u8) !void {
    var file = try std.fs.cwd().createFile(path, .{ .truncate = true });
    defer file.close();
    try file.writeAll(data);
}

fn align4(value: u32) u32 {
    return (value + 3) & ~@as(u32, 3);
}

fn getString(value: std.json.Value, key: []const u8) ?[]const u8 {
    if (value != .object) return null;
    const v = value.object.get(key) orelse return null;
    return if (v == .string) v.string else null;
}

fn getObject(value: std.json.Value, key: []const u8) ?std.json.Value {
    if (value != .object) return null;
    const v = value.object.get(key) orelse return null;
    return if (v == .object) v else null;
}

fn getArray(value: std.json.Value, key: []const u8) ?[]const std.json.Value {
    if (value != .object) return null;
    const v = value.object.get(key) orelse return null;
    return if (v == .array) v.array.items else null;
}

/// Check a type entry for the structuredefinition-fhir-type extension.
/// Returns the FHIR type name (e.g., "id") if present, null otherwise.
fn getFhirTypeExtension(type_entry: std.json.Value) ?[]const u8 {
    const extensions = getArray(type_entry, "extension") orelse return null;
    for (extensions) |ext| {
        const url = getString(ext, "url") orelse continue;
        if (std.mem.eql(u8, url, "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type")) {
            return getString(ext, "valueUrl");
        }
    }
    return null;
}

fn lastPathSegment(url: ?[]const u8) ?[]const u8 {
    const u = url orelse return null;
    if (std.mem.lastIndexOfScalar(u8, u, '/')) |idx| {
        return u[idx + 1 ..];
    }
    return u;
}

const NormalizedType = struct {
    local: ?[]const u8,
    sys_id: ?u32,
};

fn normalizeTypeCode(code: []const u8) NormalizedType {
    const prefix = "http://hl7.org/fhirpath/System.";
    if (std.mem.startsWith(u8, code, prefix)) {
        const suffix = code[prefix.len..];
        return .{ .local = null, .sys_id = systemTypeIdFromSuffix(suffix) };
    }
    if (std.mem.lastIndexOfScalar(u8, code, '/')) |idx| {
        return .{ .local = code[idx + 1 ..], .sys_id = null };
    }
    return .{ .local = code, .sys_id = null };
}

fn systemTypeId(name: []const u8) ?u32 {
    for (SystemTypes, 0..) |sys, i| {
        if (std.mem.eql(u8, name, sys)) return @intCast(i + 1);
    }
    return null;
}

fn systemTypeIdFromSuffix(suffix: []const u8) ?u32 {
    for (SystemTypes, 0..) |sys, i| {
        if (std.mem.lastIndexOfScalar(u8, sys, '.')) |idx| {
            if (std.mem.eql(u8, sys[idx + 1 ..], suffix)) return @intCast(i + 1);
        } else if (std.mem.eql(u8, sys, suffix)) {
            return @intCast(i + 1);
        }
    }
    return null;
}

fn capitalizeSuffix(allocator: std.mem.Allocator, norm: NormalizedType) ![]const u8 {
    var suffix: []const u8 = "";
    if (norm.sys_id) |sid| {
        const sys_name = SystemTypes[sid - 1];
        if (std.mem.lastIndexOfScalar(u8, sys_name, '.')) |idx| {
            suffix = sys_name[idx + 1 ..];
        } else {
            suffix = sys_name;
        }
    } else if (norm.local) |name| {
        suffix = name;
    }
    if (suffix.len == 0) return suffix;
    var out = try allocator.alloc(u8, suffix.len);
    out[0] = std.ascii.toUpper(suffix[0]);
    @memcpy(out[1..], suffix[1..]);
    return out;
}

fn toLower(allocator: std.mem.Allocator, input: []const u8) ![]const u8 {
    var out = try allocator.alloc(u8, input.len);
    for (input, 0..) |c, idx| {
        out[idx] = std.ascii.toLower(c);
    }
    return out;
}

fn printUsage() void {
    const usage =
        \\Usage: zig run scripts/build_model.zig -- [--fhir-version r5] [--in-dir path] [--out path]
        \\
        \\Builds model.bin from profiles-types.json + profiles-resources.json.
        \\
    ;
    std.debug.print("{s}", .{usage});
}

fn writeU16(buf: []u8, off: u32, value: u16) void {
    std.mem.writeInt(u16, buf[@intCast(off)..][0..2], value, .little);
}

fn writeU32(buf: []u8, off: u32, value: u32) void {
    std.mem.writeInt(u32, buf[@intCast(off)..][0..4], value, .little);
}
