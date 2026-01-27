# Zig Engine Design (Greenfield)

This document defines the greenfield Zig-based FHIRPath engine ABI and its JS wrapper interface.
It is intentionally not compatible with the existing WAT ABI. The only requirement is that the
engine can eventually pass the full test suite.

This doc is split into two parts:
1) ABI + JS wrapper interface (initial focus)
2) Engine coding/organization principles (to be expanded with you)

---

## Part 1: ABI + JS Wrapper

### Design goals
- Iterator-only results (no output modes).
- Zero JSON materialization unless the caller requests `node.data`.
- Zero-copy JSON spans when results point into the input buffer.
- Custom functions (including `trace`) provided by host as name -> host_id map.
- Schemas registered by FHIR version string; optional default schema.
- Simple, explicit memory boundaries.
- Env variable names are bare; `%name` in the expression triggers env lookup.
- Date/Time/DateTime are ISO-8601 strings and preserve original precision.
- System.Quantity is just `{ value, unit }`.

### Core types (ABI)

```zig
pub const Status = enum(u32) {
    ok = 0,
    parse_error,
    runtime_error,
    type_error,
    singleton_required,
    order_undefined,
    output_overflow,
    arena_oom,
    model_error,
    invalid_options,
    unsupported,
};

pub const Error = extern struct {
    status: Status,
    code: u32,
    detail: u32,
};

pub const DataKind = enum(u32) {
    none = 0,
    json_span = 1, // span into input buffer
    value = 2,     // arena-owned value payload (synthetic or literal)
};

pub const ValueKind = enum(u32) {
    empty = 0,
    boolean = 1,
    integer = 2,
    long = 3,
    decimal = 4,
    string = 5,
    date = 6,
    time = 7,
    dateTime = 8,
    quantity = 9,
    json_span = 10, // ABI-only: payload is a JsonSpan
};
```

### Decimal precision (scale vs norm_scale)
We keep two precision fields for decimals:
- `scale`: original precision (digits after the decimal in the literal or parsed value).
- `norm_scale`: trimmed precision after removing trailing zeros.

Why both:
- `precision()` and equivalence (`~`) rely on the original precision (`scale`).
- normalized output and comparison use `norm_scale` to avoid trailing zeros.
  If we expose decimal as a string, `toString()` should use `scale` while
  normalized materialization should use `norm_scale`.

This mirrors the current WAT engine behavior: trimming updates `norm_scale` but
preserves `scale`.

### Schema registration
Schemas are keyed by FHIR version string (no separate schema name). A default
schema may be set at registration time and is used when eval omits a version.

```zig
export fn fhirpath_ctx_register_schema(
    ctx: u32,
    fhir_ver_ptr: u32,
    fhir_ver_len: u32,
    model_ptr: u32,
    model_len: u32,
    is_default: u32, // 0 or 1
) Status;
```

Behavior:
- If `is_default=1`, this schema becomes the default (last one wins).
- If `fhir_ver_len=0`, return `Status.invalid_options`.

### Evaluation
```zig
export fn fhirpath_eval(
    ctx: u32,
    expr_ptr: u32,
    expr_len: u32,
    json_ptr: u32,
    json_len: u32,
    fhir_ver_ptr: u32, // version string
    fhir_ver_len: u32,
    opts_ptr: u32,
    opts_len: u32,
) Status;
```

Behavior:
- If `fhir_ver_len=0`, use default schema if set; otherwise return `Status.model_error`.
- If `opts_len=0`, no env and no custom functions are provided.

### Result iteration (iterator-only)
```zig
export fn fhirpath_result_count(ctx: u32) u32;
export fn fhirpath_result_iter_new(ctx: u32) u32;
export fn fhirpath_result_iter_next(ctx: u32, iter: u32) u32; // 0 => end
```

### Item accessors
Metadata is cheap; data access is lazy.

```zig
export fn fhirpath_item_data_kind(ctx: u32, item: u32) u32; // DataKind
export fn fhirpath_item_value_kind(ctx: u32, item: u32) u32; // ValueKind for DataKind.value
export fn fhirpath_item_type_id(ctx: u32, item: u32) u32;
export fn fhirpath_item_source_pos(ctx: u32, item: u32) u32; // 0 if none
export fn fhirpath_item_source_end(ctx: u32, item: u32) u32;

// JSON span access (valid only for DataKind.json_span)
export fn fhirpath_item_json_pos(ctx: u32, item: u32) u32;
export fn fhirpath_item_json_end(ctx: u32, item: u32) u32;

// Scalar accessors (valid only for DataKind.value)
export fn fhirpath_item_bool(ctx: u32, item: u32) u32;
export fn fhirpath_item_i64(ctx: u32, item: u32) i64;
export fn fhirpath_item_str_ptr(ctx: u32, item: u32) u32;
export fn fhirpath_item_str_len(ctx: u32, item: u32) u32;
export fn fhirpath_item_decimal(ctx: u32, item: u32, out_ptr: u32) Status;
export fn fhirpath_item_quantity(ctx: u32, item: u32, out_ptr: u32) Status;
```

### Item layout (conceptual, internal)
Items are internal and not part of the ABI. We use named fields rather than
generic `a/b/c` payloads.

```
Item
+-------------------+
| data_kind (u32)   |  0=none 1=json_span 2=value
| value_kind (u32)  |  ValueKind for data_kind=value
| type_id (u32)     |
| source_pos (u32)  |
| source_end (u32)  |
| data_pos (u32)    |  json_span: pos
| data_end (u32)    |  json_span: end
| value_ptr (u32)   |  value: ptr to Value (only valid if data_kind=value)
+-------------------+
```

Notes:
- For `json_span`, `data_pos` and `data_end` are set; `value_ptr` is unused.
- For `value`, `value_ptr` is set; `data_pos/data_end` are unused.

Accessor output structs:
```zig
pub const DecimalOut = extern struct {
    sign: i32,       // -1 or 1
    scale: u32,      // original precision
    norm_scale: u32, // trimmed precision
    mag0: u32,
    mag1: u32,
    mag2: u32,
    mag3: u32,       // u128 magnitude, little-endian limbs
};

pub const QuantityOut = extern struct {
    dec: DecimalOut,
    unit_ptr: u32,   // UTF-8 unit string in engine memory
    unit_len: u32,
};
```

### Internal value model (idiomatic Zig)
Use a tagged union internally with named payloads. The ABI blob below is only for
interop; internal code should not use `a/b/c/d`-style fields.

```zig
const Value = union(enum) {
    empty,
    boolean: bool,
    integer: i32,
    long: i64,
    decimal: *Decimal,
    string: Slice,
    date: Slice,     // ISO-8601, preserves precision
    time: Slice,     // ISO-8601, preserves precision
    dateTime: Slice, // ISO-8601, preserves precision
    quantity: Quantity,
    json_span: JsonSpan,
};

const Decimal = struct {
    sign: i8,
    scale: u8,       // original precision
    norm_scale: u8,  // trimmed precision
    mag: [4]u32,     // u128 magnitude, little-endian limbs
};

const Quantity = struct {
    value: *Decimal,
    unit: Slice,
};

const Slice = struct { ptr: u32, len: u32 };
const JsonSpan = struct { pos: u32, end: u32 };
```

### ABI value blob (extern, stable layout)
Use a compact ABI blob for env values and custom function returns. The `extern union`
is an ABI overlay; it is not meant for internal logic.

```zig
const ValueBlob = extern struct {
    kind: ValueKind,
    type_id: u32,
    origin_plus1: u32, // 0 => none
    payload: ValuePayload,
};

const ValuePayload = extern union {
    boolean: u32,
    i64: I64Parts,
    slice: Slice,
    dec_ptr: u32,          // Decimal payload pointer
    qty: QuantityBlob,     // value + unit
    json: JsonSpan,        // input span
};

const I64Parts = extern struct { lo: u32, hi: u32 };
const Slice = extern struct { ptr: u32, len: u32 };
const JsonSpan = extern struct { pos: u32, end: u32 };
const QuantityBlob = extern struct {
    dec_ptr: u32,
    unit_ptr: u32,
    unit_len: u32,
};
```

### Options blob (no presence flags)
Sections are present if their lengths are > 0.

```zig
pub const Options = extern struct {
    magic: u32,   // 'FP30'
    version: u32, // ABI version
    policy_flags: u32,
    debug_flags: u32,
    env_offset: u32,
    env_len: u32,
    func_offset: u32,
    func_len: u32,
};
```

### Env blob
Env variables are passed as explicit values or JSON spans. The JS wrapper will
build this blob automatically. Names are bare (no `%`).

```zig
pub const EnvHeader = extern struct { count: u32 };

pub const EnvValueKind = enum(u32) {
    scalar = 1,
    json_span = 2,
    collection = 3,
};

pub const EnvEntry = extern struct {
    name_off: u32,
    name_len: u32,
    value_off: u32,  // ValueBlob or CollectionValue depending on value_kind
    value_kind: u32, // EnvValueKind
};

pub const CollectionValue = extern struct {
    count: u32,
    items_off: u32, // array of ValueBlob offsets
};
```

Notes:
- If `value_kind` is scalar or json_span, `value_off` points to a `ValueBlob`.
- If `value_kind` is collection, `value_off` points to `CollectionValue`, which
  references an array of `ValueBlob` offsets (each item can be scalar or json_span).

### Custom functions
Host provides a name -> host_id table. `trace` is just another custom function.

```zig
pub const FuncHeader = extern struct { count: u32 };

pub const FuncEntry = extern struct {
    name_off: u32,
    name_len: u32,
    arity_min: u8,
    arity_max: u8, // 255 = variadic
    _pad: u16,
    host_id: u32,
};
```

Host import:
```zig
extern fn host_call(
    ctx: u32,
    host_id: u32,
    argc: u32,
    argv_ptr: u32, // array of collection handles
    out_ptr: u32,
    out_max: u32,
) i32;
```

Call convention:
- `argv[0]` is the base collection (the receiver).
- `argv[1..]` are fully evaluated argument collections.
- `trace()` returns `argv[0]`.

Host return contract:
```zig
pub const HostResultKind = enum(u32) {
    return_arg = 0, // return argv[index]
    value_blob = 1, // return ValueBlob at data offset
    empty = 2,      // return empty collection
};

pub const HostResult = extern struct {
    kind: u32, // HostResultKind
    data: u32, // arg index or byte offset from out_ptr to ValueBlob
};
```

For `value_blob`, the host writes a `ValueBlob` inside the output buffer
starting at `out_ptr + data`. The engine copies it into its arena before
returning a result.

### JS wrapper interface (lazy getters)

Minimal public JS API:
```js
const engine = await FhirPathEngine.instantiate(wasm, { functions });
engine.registerSchema({ fhirVersion: "5.0.0", model, isDefault: true });

const res = engine.eval({ expr, json, fhirVersion: "5.0.0", env });
for (const node of res) {
  console.log(node.meta.typeName);
  // data is lazy, only decoded if accessed
  if (needValue) console.log(node.data);
}
```

Node wrapper (lazy getters):
```js
class FhirPathNode {
  constructor(engine, item) {
    this.engine = engine;
    this.item = item;
    this._data = undefined;
    this._meta = undefined;
  }

  get data() {
    if (this._data !== undefined) return this._data;
    const kind = this.engine._itemDataKind(this.item);

    if (kind === 1) { // json_span
      const { pos, end } = this.engine._itemJsonSpan(this.item);
      const bytes = this.engine._mem.subarray(pos, end);
      const text = this.engine._decoder.decode(bytes);
      this._data = JSON.parse(text); // cost only on access
      return this._data;
    }

    if (kind === 2) { // value payload
      this._data = this.engine._decodeScalarValue(this.item);
      return this._data;
    }

    this._data = null;
    return this._data;
  }

  get meta() {
    if (this._meta) return this._meta;
    const typeId = this.engine._itemTypeId(this.item);
    this._meta = {
      typeId,
      typeName: this.engine._typeNameFromId(typeId),
      source: this.engine._itemSourceSpan(this.item),
    };
    return this._meta;
  }
}
```

### Memory layout diagrams (conceptual)

Input-backed JSON span (zero-copy):
```
WASM memory
+---------------------------------------------------------+
| input JSON buffer ... "dateTime":"2020-01-01T..."      |
+----------------------------^----------------------------+
                             |
Item.data_kind = json_span   |
Item.data_pos  = pos --------+
Item.data_end  = end
```

Synthetic dateTime (arena value):
```
Arena memory
+------------------------------+
| "2020-01-01T12:30:45Z" bytes |
+------------------------------+
        ^
        |
Value.dateTime Slice {ptr,len}
        ^
        |
Item.data_kind = value
Item.value_ptr = ptr to Value
Item.value_kind = dateTime
```

### Realistic usage examples

Trace as custom function:
```js
const engine = await FhirPathEngine.instantiate(wasm, {
  functions: {
    trace({ args, helper }) {
      const name = helper.firstString(args[1]) ?? "trace";
      const value = args[2] ?? args[0];
      console.log("[trace]", name, helper.toJS(value));
      return { returnArg: 0 };
    },
  },
});
```

Synthetic value (e.g., `toQuantity()`):
- Engine returns DataKind.value with ValueKind.quantity and arena payload.
- `node.data` decodes the payload into `{ value, unit }`.

Input-backed JSON span:
- Engine returns DataKind.json_span pointing to the input buffer.
- `node.data` decodes only when accessed.

---

## Testing & Debugging Plan (Zig-First)

We will run the full test suite in native Zig first (non-wasm) to maximize
debug visibility, error reporting, and introspection. The wasm boundary should
be crossed only after core semantics are validated in Zig.

Implications:
- Build a Zig-native test harness for spec tests and artisinal tests.
- Provide rich error context (token spans, AST dumps, eval traces) in Zig.
- Mirror those tests in wasm only after behavior is stable.

### Zig 0.15 notes (for contributors)
- `std.ArrayList(T)` is unmanaged; use `.empty` and pass an allocator to
  `append`, `appendSlice`, `deinit`, `clearAndFree`, and `toOwnedSlice`.
- `ArrayList(u8).writer(allocator)` takes an allocator in 0.15.
- `std.json.Stringify.valueAlloc` replaces older `stringifyAlloc`.
- `std.json.Value.array` uses `std.json.Array` (managed list); when fabricating
  a temporary array, fill `{ .items = slice, .capacity = slice.len, .allocator = allocator }`.
- Build scripts use the new module API:
  - `b.createModule(.{ .root_source_file = b.path(\"src/lib.zig\"), ... })`
  - `b.addTest(.{ .root_module = module })`
  - `b.addExecutable(.{ .root_module = module, ... })`
- For simple stderr/stdout output in tools, `std.debug.print` is the most stable
  option across recent Zig versions.

## Model Blob Format (FHIR R4/R5)

We use a compact, self-contained model blob so the engine can resolve:
- type name -> type id
- type id -> type info (name, base, kind)
- field lookup by name for a given type
- choice types (`[x]`) and variant resolution

This is similar to the current WAT model blob but with clearer per-type field
ranges and a sorted name index.

The blob is intentionally simple; the engine may build any additional indexes
in memory at load time. On-disk indexes are optional and versioned via the
header, so we can evolve the format later without breaking callers.

### Header (fixed size, little-endian)

```
struct ModelHeader {
  u32 magic;            // "FPM4"
  u16 version;          // 1
  u16 header_size;      // bytes
  u32 total_size;       // bytes
  u32 strings_off;      // string pool start
  u32 strings_len;      // string pool length
  u32 types_off;        // type table start
  u32 types_count;
  u32 name_index_off;   // sorted type-name index
  u32 name_index_count;
  u32 fields_off;       // field table start
  u32 fields_count;
  u32 choice_groups_off;
  u32 choice_groups_count;
  u32 choice_variants_off;
  u32 choice_variants_count;
  u32 flags;            // kind + family (R4/R5) + compat markers
}
```

All offsets are relative to `model_ptr`. All sections are 4-byte aligned.

### String Pool

Raw UTF-8 bytes, not null-terminated. All names are referenced via
`(name_off, name_len)` pairs from tables.

### Type IDs

System type ids are fixed and not stored in the model:
`System.Boolean`, `System.Integer`, etc.

Model type ids are computed as:
```
type_id = 0x80000000 | (type_index + 1)
```

### Type Table

```
struct TypeEntry {
  u32 name_off;
  u16 name_len;
  u8  kind;           // system/primitive/complex/resource/backbone
  u8  flags;          // reserved
  u32 base_type_id;   // 0 if none
  u32 prim_base_id;   // 0 if none (for primitives)
  u32 field_start;    // index into FieldEntry table
  u32 field_count;
}
```

Type names for backbone elements use path-like names (e.g., `Encounter.reason`)
so metadata can surface the real backbone type.

### Type Name Index (sorted)

```
struct NameIndexEntry {
  u32 name_off;
  u32 name_len;
  u32 type_index; // 0-based into TypeEntry table
}
```

The name index is sorted by string bytes to allow binary search without a hash
table. If `name_index_count` is 0, the engine may build an index at load time.
(Hashing can be added later without changing the core tables.)

### Field Table (per-type ranges)

Field entries are sorted by `name` within each type’s range when possible, but
the engine may fall back to linear scan or build per-type indexes at load time.

```
struct FieldEntry {
  u32 name_off;
  u32 name_len;
  u32 child_type_id;
  u32 flags;          // MULTIPLE, CHOICE_BASE, CHOICE_VARIANT
  u32 choice_group_id; // 0 if none
}
```

Lookup algorithm:
1) Get type’s `field_start/field_count`.
2) Binary search by name in that range.
3) If not found, walk `base_type_id` and repeat.

### Choice Groups + Variants

Each choice group is specific to a type+field, not global.

```
struct ChoiceGroupEntry {
  u32 base_name_off;
  u32 base_name_len;
  u32 variant_start;  // index into ChoiceVariant table
  u32 variant_count;
}

struct ChoiceVariantEntry {
  u32 name_off;       // e.g., "valueString"
  u32 name_len;
  u32 type_id;
}
```

Resolution:
- If the expression matches a choice base field, the group lists its variants.
- If the expression matches a variant name, the field entry is flagged as
  `CHOICE_VARIANT` and points at the same group.

### Flags

Header `flags` includes:
- Model kind: generic vs FHIR
- Family: R4 or R5
- Compatibility marker (optional)

Field `flags` includes:
- `MULTIPLE` (array)
- `CHOICE_BASE`
- `CHOICE_VARIANT`

### Why this layout?

- Deterministic, compact, and easy to parse in wasm.
- Fast field lookup via per-type sorted ranges.
- Explicit choice metadata without hardcoded FHIR rules.

---

## Part 2: Engine Principles (Placeholder)

We will iterate on these with you. Proposed topics:
- Memory/arena model and lifetime.
- AST, value, and collection representations.
- Parser and eval architecture.
- Schema/model access and type inference.
- Custom function invocation strategy.
- Error handling and tracing.
