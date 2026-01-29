const std = @import("std");

pub const SystemTypeNames = [_][]const u8{
    "System.Any",
    "System.Boolean",
    "System.Integer",
    "System.Long",
    "System.Decimal",
    "System.String",
    "System.Date",
    "System.DateTime",
    "System.Time",
    "System.Quantity",
    "System.TypeInfo",
};

pub fn systemTypeName(id: u32) []const u8 {
    if (id == 0) return "";
    const idx: usize = @intCast(id - 1);
    if (idx >= SystemTypeNames.len) return "";
    return SystemTypeNames[idx];
}

pub fn systemTypeId(name: []const u8) ?u32 {
    var i: usize = 0;
    while (i < SystemTypeNames.len) : (i += 1) {
        if (std.mem.eql(u8, name, SystemTypeNames[i])) return @intCast(i + 1);
    }
    return null;
}

pub fn isModelType(id: u32) bool {
    return (id & 0x8000_0000) != 0;
}

pub fn modelIndex(id: u32) u32 {
    return (id & 0x7fff_ffff) - 1;
}

pub const ModelHeader = struct {
    version: u16,
    header_size: u16,
    total_size: u32,
    strings_off: u32,
    strings_len: u32,
    types_off: u32,
    types_count: u32,
    name_index_off: u32,
    name_index_count: u32,
    fields_off: u32,
    fields_count: u32,
    choice_groups_off: u32,
    choice_groups_count: u32,
    choice_variants_off: u32,
    choice_variants_count: u32,
    flags: u32,
};

pub const TypeEntry = struct {
    name_off: u32,
    name_len: u16,
    kind: u8,
    flags: u8,
    base_type_id: u32,
    prim_base_id: u32,
    field_start: u32,
    field_count: u32,
};

pub const NameIndexEntry = struct {
    name_off: u32,
    name_len: u32,
    type_index: u32,
};

pub const FieldEntry = struct {
    name_off: u32,
    name_len: u16,
    child_type_id: u32,
    flags: u8,
    choice_group_id: u16,
};

// Field flags
pub const FIELD_MULTIPLE: u8 = 1 << 0;
pub const FIELD_CHOICE_BASE: u8 = 1 << 1;
pub const FIELD_CHOICE_VARIANT: u8 = 1 << 2;

pub const ChoiceGroupEntry = struct {
    base_name_off: u32,
    base_name_len: u32,
    variant_start: u32,
    variant_count: u32,
};

pub const ChoiceVariantEntry = struct {
    name_off: u32,
    name_len: u32,
    type_id: u32,
};

pub const Model = struct {
    bytes: []const u8,
    header: ModelHeader,
    strings: []const u8,

    pub fn init(bytes: []const u8) !Model {
        if (bytes.len < 64) return error.InvalidModel;
        if (!std.mem.eql(u8, bytes[0..4], "FPM4")) return error.InvalidModel;
        const version = readU16(bytes, 4);
        const header_size = readU16(bytes, 6);
        if (header_size < 64 or header_size > bytes.len) return error.InvalidModel;
        const header = ModelHeader{
            .version = version,
            .header_size = header_size,
            .total_size = readU32(bytes, 8),
            .strings_off = readU32(bytes, 12),
            .strings_len = readU32(bytes, 16),
            .types_off = readU32(bytes, 20),
            .types_count = readU32(bytes, 24),
            .name_index_off = readU32(bytes, 28),
            .name_index_count = readU32(bytes, 32),
            .fields_off = readU32(bytes, 36),
            .fields_count = readU32(bytes, 40),
            .choice_groups_off = readU32(bytes, 44),
            .choice_groups_count = readU32(bytes, 48),
            .choice_variants_off = readU32(bytes, 52),
            .choice_variants_count = readU32(bytes, 56),
            .flags = readU32(bytes, 60),
        };
        if (header.total_size != 0 and header.total_size > bytes.len) return error.InvalidModel;
        const strings_end = header.strings_off + header.strings_len;
        if (strings_end > bytes.len) return error.InvalidModel;
        return .{ .bytes = bytes, .header = header, .strings = bytes[header.strings_off..strings_end] };
    }

    pub fn typeEntry(self: *const Model, idx: u32) TypeEntry {
        const off = self.header.types_off + idx * 24;
        return .{
            .name_off = readU32(self.bytes, off),
            .name_len = readU16(self.bytes, off + 4),
            .kind = self.bytes[off + 6],
            .flags = self.bytes[off + 7],
            .base_type_id = readU32(self.bytes, off + 8),
            .prim_base_id = readU32(self.bytes, off + 12),
            .field_start = readU32(self.bytes, off + 16),
            .field_count = readU32(self.bytes, off + 20),
        };
    }

    pub fn nameIndexEntry(self: *const Model, idx: u32) NameIndexEntry {
        const off = self.header.name_index_off + idx * 12;
        return .{
            .name_off = readU32(self.bytes, off),
            .name_len = readU32(self.bytes, off + 4),
            .type_index = readU32(self.bytes, off + 8),
        };
    }

    pub fn fieldEntry(self: *const Model, idx: u32) FieldEntry {
        if (self.header.version >= 2) {
            // Packed format: 12 bytes per entry
            const off = self.header.fields_off + idx * 12;
            const flags_and_choice = readU16(self.bytes, off + 6);
            return .{
                .name_off = readU32(self.bytes, off),
                .name_len = readU16(self.bytes, off + 4),
                .flags = @intCast((flags_and_choice >> 12) & 0x0F),
                .choice_group_id = flags_and_choice & 0x0FFF,
                .child_type_id = readU32(self.bytes, off + 8),
            };
        } else {
            // Legacy format: 20 bytes per entry
            const off = self.header.fields_off + idx * 20;
            return .{
                .name_off = readU32(self.bytes, off),
                .name_len = @intCast(readU32(self.bytes, off + 4)),
                .child_type_id = readU32(self.bytes, off + 8),
                .flags = @intCast(readU32(self.bytes, off + 12)),
                .choice_group_id = @intCast(readU32(self.bytes, off + 16)),
            };
        }
    }

    pub fn choiceGroupEntry(self: *const Model, idx: u32) ChoiceGroupEntry {
        const off = self.header.choice_groups_off + idx * 16;
        return .{
            .base_name_off = readU32(self.bytes, off),
            .base_name_len = readU32(self.bytes, off + 4),
            .variant_start = readU32(self.bytes, off + 8),
            .variant_count = readU32(self.bytes, off + 12),
        };
    }

    pub fn choiceVariantEntry(self: *const Model, idx: u32) ChoiceVariantEntry {
        const off = self.header.choice_variants_off + idx * 12;
        return .{
            .name_off = readU32(self.bytes, off),
            .name_len = readU32(self.bytes, off + 4),
            .type_id = readU32(self.bytes, off + 8),
        };
    }

    pub fn stringAt(self: *const Model, off: u32, len: u32) []const u8 {
        const start: usize = @intCast(off);
        const len_usize: usize = @intCast(len);
        const end: usize = start + len_usize;
        if (end > self.strings.len) return "";
        return self.strings[start..end];
    }

    pub fn typeName(self: *const Model, idx: u32) []const u8 {
        const t = self.typeEntry(idx);
        return self.stringAt(t.name_off, t.name_len);
    }

    pub fn fieldName(self: *const Model, idx: u32) []const u8 {
        const f = self.fieldEntry(idx);
        return self.stringAt(f.name_off, f.name_len);
    }

    pub fn findTypeIndexByName(self: *const Model, name: []const u8) ?u32 {
        var lo: u32 = 0;
        var hi: u32 = self.header.name_index_count;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            const entry = self.nameIndexEntry(mid);
            const entry_name = self.stringAt(entry.name_off, entry.name_len);
            switch (std.mem.order(u8, entry_name, name)) {
                .lt => lo = mid + 1,
                .gt => hi = mid,
                .eq => return entry.type_index,
            }
        }
        return null;
    }
};

pub const Schema = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    prefix: []const u8,
    model: Model,
    full_names: [][]const u8,

    pub fn init(
        allocator: std.mem.Allocator,
        name: []const u8,
        prefix: []const u8,
        model_bytes: []const u8,
    ) !Schema {
        const model = try Model.init(model_bytes);
        const name_copy = try allocator.dupe(u8, name);
        const prefix_copy = try allocator.dupe(u8, prefix);
        var full_names = try allocator.alloc([]const u8, model.header.types_count);
        var i: u32 = 0;
        while (i < model.header.types_count) : (i += 1) {
            const local = model.typeName(i);
            if (prefix_copy.len == 0) {
                full_names[i] = try allocator.dupe(u8, local);
            } else {
                full_names[i] = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ prefix_copy, local });
            }
        }
        return .{
            .allocator = allocator,
            .name = name_copy,
            .prefix = prefix_copy,
            .model = model,
            .full_names = full_names,
        };
    }

    pub fn deinit(self: *Schema) void {
        for (self.full_names) |name| self.allocator.free(name);
        self.allocator.free(self.full_names);
        self.allocator.free(self.name);
        self.allocator.free(self.prefix);
    }

    pub fn typeName(self: *Schema, id: u32) []const u8 {
        if (id == 0) return "";
        if (!isModelType(id)) return systemTypeName(id);
        const idx = modelIndex(id);
        if (idx >= self.full_names.len) return "";
        return self.full_names[idx];
    }

    /// Get the implicit System type ID for a model type (FHIR primitive → System type).
    /// The mapping is data-driven from the model blob's prim_base_id field.
    /// Returns 0 if the type has no implicit System type conversion.
    pub fn primBaseId(self: *Schema, id: u32) u32 {
        if (!isModelType(id)) return 0;
        const idx = modelIndex(id);
        if (idx >= self.model.header.types_count) return 0;
        const t = self.model.typeEntry(idx);
        return t.prim_base_id;
    }

    pub fn typeIdByLocalName(self: *Schema, name: []const u8) ?u32 {
        const idx = self.model.findTypeIndexByName(name) orelse return null;
        return 0x8000_0000 | (idx + 1);
    }

    /// Find type by qualified name (e.g., "FHIR.Patient")
    /// Only matches if the prefix matches this schema's prefix (e.g., "FHIR")
    pub fn typeIdByQualifiedName(self: *Schema, name: []const u8) ?u32 {
        if (std.mem.indexOf(u8, name, ".")) |dot| {
            const prefix = name[0..dot];
            const local_name = name[dot + 1 ..];
            // Only match if the prefix matches our schema's prefix
            if (std.mem.eql(u8, prefix, self.prefix)) {
                return self.typeIdByLocalName(local_name);
            }
            // Different namespace prefix - no match
            return null;
        }
        // No prefix - just look up by local name
        return self.typeIdByLocalName(name);
    }

    /// Map a model type to its implicit FHIRPath System type.
    /// Uses the prim_base_id field from the model blob (computed at build time).
    /// Returns 0 if no implicit conversion exists.
    pub fn implicitSystemTypeId(self: *Schema, type_id: u32) u32 {
        return self.primBaseId(type_id);
    }

    /// Resolve the output type ID for a result item.
    /// FHIR primitives are downcast to their System type IDs;
    /// complex FHIR types and System types pass through unchanged.
    pub fn outputTypeId(self: *Schema, type_id: u32) u32 {
        if (isModelType(type_id)) {
            const sys_id = self.implicitSystemTypeId(type_id);
            if (sys_id != 0) return sys_id;
        }
        return type_id;
    }

    /// Resolve the output type name for a result item.
    /// FHIR primitives are downcast to their System type names;
    /// complex FHIR types keep their qualified name.
    pub fn outputTypeName(self: *Schema, type_id: u32) []const u8 {
        if (type_id == 0) return "";
        if (isModelType(type_id)) {
            const sys_id = self.implicitSystemTypeId(type_id);
            if (sys_id != 0) return systemTypeName(sys_id);
            return self.typeName(type_id);
        }
        return systemTypeName(type_id);
    }

    /// Check if type_id is a subtype of target_type_id (walks inheritance chain)
    pub fn isSubtype(self: *Schema, type_id: u32, target_type_id: u32) bool {
        if (type_id == target_type_id) return true;
        if (!isModelType(type_id)) return false;

        var current = type_id;
        while (true) {
            const idx = modelIndex(current);
            if (idx >= self.model.header.types_count) return false;
            const t = self.model.typeEntry(idx);
            if (t.base_type_id == 0) return false;
            if (t.base_type_id == target_type_id) return true;
            current = t.base_type_id;
        }
    }

    pub fn fieldForType(self: *Schema, type_id: u32, field_name: []const u8) ?FieldEntry {
        if (!isModelType(type_id)) return null;
        var current = type_id;
        while (true) {
            const idx = modelIndex(current);
            if (idx >= self.model.header.types_count) return null;
            const t = self.model.typeEntry(idx);
            if (t.field_count > 0) {
                if (findFieldInRange(self, t.field_start, t.field_count, field_name)) |entry| {
                    return entry;
                }
            }
            if (t.base_type_id == 0) break;
            current = t.base_type_id;
        }
        return null;
    }

    pub fn childTypeForField(self: *Schema, type_id: u32, field_name: []const u8) ?u32 {
        const entry = self.fieldForType(type_id, field_name) orelse return null;
        if (entry.child_type_id == 0) return null;
        // FHIR primitives have an implicit .value child whose type is the
        // corresponding System type (e.g. FHIR.code.value -> System.String).
        if (std.mem.eql(u8, field_name, "value")) {
            const sys_id = self.implicitSystemTypeId(type_id);
            if (sys_id != 0) return sys_id;
        }
        return entry.child_type_id;
    }

    /// Check if a field is a choice base type and return its choice_group_id.
    /// Returns null if the field doesn't exist or isn't a choice base.
    pub fn choiceGroupForField(self: *Schema, type_id: u32, field_name: []const u8) ?u16 {
        const entry = self.fieldForType(type_id, field_name) orelse return null;
        if ((entry.flags & FIELD_CHOICE_BASE) != 0 and entry.choice_group_id > 0) {
            return entry.choice_group_id;
        }
        return null;
    }

    /// Get all choice variant names for a given choice group within a type.
    /// Returns an iterator over (variant_name, child_type_id) pairs.
    pub fn choiceVariantsForType(self: *Schema, type_id: u32, choice_group_id: u16) ChoiceVariantIterator {
        return .{
            .schema = self,
            .type_id = type_id,
            .choice_group_id = choice_group_id,
        };
    }

    pub const ChoiceVariantIterator = struct {
        schema: *Schema,
        type_id: u32,
        choice_group_id: u16,
        current_type_id: ?u32 = null,
        field_idx: u32 = 0,
        field_end: u32 = 0,
        started: bool = false,

        pub fn next(self: *ChoiceVariantIterator) ?struct { name: []const u8, child_type_id: u32 } {
            if (!self.started) {
                self.started = true;
                self.current_type_id = self.type_id;
                if (isModelType(self.type_id)) {
                    const idx = modelIndex(self.type_id);
                    if (idx < self.schema.model.header.types_count) {
                        const t = self.schema.model.typeEntry(idx);
                        self.field_idx = t.field_start;
                        self.field_end = t.field_start + t.field_count;
                    }
                }
            }

            while (self.current_type_id) |tid| {
                while (self.field_idx < self.field_end) {
                    const entry = self.schema.model.fieldEntry(self.field_idx);
                    self.field_idx += 1;

                    // Check if this is a variant for our choice group
                    if (entry.choice_group_id == self.choice_group_id and
                        (entry.flags & FIELD_CHOICE_VARIANT) != 0)
                    {
                        return .{
                            .name = self.schema.model.stringAt(entry.name_off, entry.name_len),
                            .child_type_id = entry.child_type_id,
                        };
                    }
                }

                // Move to base type
                const idx = modelIndex(tid);
                if (idx >= self.schema.model.header.types_count) break;
                const t = self.schema.model.typeEntry(idx);
                if (t.base_type_id == 0 or !isModelType(t.base_type_id)) {
                    self.current_type_id = null;
                    break;
                }
                self.current_type_id = t.base_type_id;
                const base_idx = modelIndex(t.base_type_id);
                if (base_idx >= self.schema.model.header.types_count) {
                    self.current_type_id = null;
                    break;
                }
                const base_t = self.schema.model.typeEntry(base_idx);
                self.field_idx = base_t.field_start;
                self.field_end = base_t.field_start + base_t.field_count;
            }

            return null;
        }
    };
};

fn findFieldInRange(self: *Schema, start: u32, count: u32, name: []const u8) ?FieldEntry {
    var lo: u32 = 0;
    var hi: u32 = count;
    while (lo < hi) {
        const mid = lo + (hi - lo) / 2;
        const idx = start + mid;
        const entry = self.model.fieldEntry(idx);
        const entry_name = self.model.stringAt(entry.name_off, entry.name_len);
        switch (std.mem.order(u8, entry_name, name)) {
            .lt => lo = mid + 1,
            .gt => hi = mid,
            .eq => return entry,
        }
    }
    var i: u32 = 0;
    while (i < count) : (i += 1) {
        const idx = start + i;
        const entry = self.model.fieldEntry(idx);
        const entry_name = self.model.stringAt(entry.name_off, entry.name_len);
        if (std.mem.eql(u8, entry_name, name)) return entry;
    }
    return null;
}

fn readU16(bytes: []const u8, off: usize) u16 {
    const ptr = @as(*const [2]u8, @ptrCast(bytes[off..].ptr));
    return std.mem.readInt(u16, ptr, .little);
}

fn readU32(bytes: []const u8, off: usize) u32 {
    const ptr = @as(*const [4]u8, @ptrCast(bytes[off..].ptr));
    return std.mem.readInt(u32, ptr, .little);
}
