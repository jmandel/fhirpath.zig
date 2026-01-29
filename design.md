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
- Zero-copy JSON text slices when the adapter can expose them.
- Custom functions (including `trace`) provided by host as name -> host_id map.
- Schemas registered by (name, prefix); optional default schema.
- All non-System types are schema-defined at runtime (no hardcoded FHIR enums).
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
    node_ref = 1, // node-backed item; adapter decides how to resolve data
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
    json_text = 10, // ABI-only: payload is a Slice to JSON text
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
Schemas are keyed by a **name** (e.g. `"r4"`, `"r5"`, or `"custom"`) and a
**prefix** (e.g. `"FHIR"`). The prefix is used to construct fully qualified
type names like `FHIR.Patient` or `FHIR.HumanName`. A default schema may be set
at registration time and is used when eval omits a schema name.

Type IDs are **schema-local**. The high bit (`0x80000000`) marks a model type
id; the remaining bits are an index into that schema’s Type table. The same
numeric type id may refer to different types across schemas, so callers must
interpret `type_id` in the context of the selected schema for a given eval.

```zig
export fn fhirpath_ctx_register_schema(
    ctx: u32,
    schema_name_ptr: u32,
    schema_name_len: u32,
    prefix_ptr: u32,
    prefix_len: u32,
    model_ptr: u32,
    model_len: u32,
    is_default: u32, // 0 or 1
) Status;

// Set current time (Unix epoch seconds) used by now(), today(), timeOfDay().
export fn fhirpath_ctx_set_time(ctx: u32, epoch_seconds: i64) Status;
```

Behavior:
- If `is_default=1`, this schema becomes the default (last one wins).
- If `schema_name_len=0`, return `Status.invalid_options`.

### Evaluation
```zig
export fn fhirpath_eval(
    ctx: u32,
    expr_ptr: u32,
    expr_len: u32,
    json_ptr: u32,
    json_len: u32,
    schema_name_ptr: u32, // schema name (e.g., "r5")
    schema_name_len: u32,
    opts_ptr: u32,
    opts_len: u32,
) Status;
```

Behavior:
- If `schema_name_len=0`, use default schema if set; otherwise return `Status.model_error`.
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

// Node-backed data access (valid only for DataKind.node_ref)
// Returns a pointer+len to JSON text (may point into input buffer or engine-owned scratch).
export fn fhirpath_item_data_ptr(ctx: u32, item: u32) u32;
export fn fhirpath_item_data_len(ctx: u32, item: u32) u32;

// Scalar accessors (valid only for DataKind.value)
export fn fhirpath_item_bool(ctx: u32, item: u32) u32;
export fn fhirpath_item_i64(ctx: u32, item: u32) i64;
export fn fhirpath_item_str_ptr(ctx: u32, item: u32) u32;
export fn fhirpath_item_str_len(ctx: u32, item: u32) u32;
export fn fhirpath_item_decimal(ctx: u32, item: u32, out_ptr: u32) Status;
export fn fhirpath_item_quantity(ctx: u32, item: u32, out_ptr: u32) Status;

// Type name lookup (returns UTF-8 slice; empty if unknown)
export fn fhirpath_type_name_ptr(ctx: u32, type_id: u32) u32;
export fn fhirpath_type_name_len(ctx: u32, type_id: u32) u32;
```

Notes:
- For wasm, `data_ptr` is a linear-memory offset. The buffer is valid until results
  are cleared (next eval or context reset).
- `data_len=0` means the adapter cannot provide JSON text; callers should not assume
  JSON is always materializable from a node reference.
- `type_name_ptr/len` returns `System.*` for system types and schema-qualified names
  (e.g., `FHIR.Patient`) for model types. The name is resolved against the schema
  used in the most recent `eval` (or default schema if used).

### Item layout (conceptual, internal)
Items are internal and not part of the ABI. We use named fields rather than
generic `a/b/c` payloads.

```
Item
+-------------------+
| data_kind (u32)   |  0=none 1=node_ref 2=value
| value_kind (u32)  |  ValueKind for data_kind=value
| type_id (u32)     |
| source_pos (u32)  |
| source_end (u32)  |
| node_ref (usize)  |  adapter-opaque node handle (index or pointer)
| value_ptr (u32)   |  value: ptr to Value (only valid if data_kind=value)
+-------------------+
```

Notes:
- For `node_ref`, the handle is adapter-defined; the engine uses the adapter to
  resolve data or metadata. `value_ptr` is unused.
- For `value`, `value_ptr` is set; JSON text access is unsupported (data_len=0).
- `source_pos/source_end` are optional and derived from `adapter.span()` when available.

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
    json_text: Slice, // ABI-only: raw JSON text passed via env/host
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
    json: Slice,           // raw JSON text
};

const I64Parts = extern struct { lo: u32, hi: u32 };
const Slice = extern struct { ptr: u32, len: u32 };
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
Env variables are passed as explicit values or JSON text. The JS wrapper will
build this blob automatically. Names are bare (no `%`).

```zig
pub const EnvHeader = extern struct { count: u32 };

pub const EnvValueKind = enum(u32) {
    scalar = 1,
    json_text = 2,
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
- If `value_kind` is scalar or json_text, `value_off` points to a `ValueBlob`.
- If `value_kind` is collection, `value_off` points to `CollectionValue`, which
  references an array of `ValueBlob` offsets (each item can be scalar or json_text).
- `json_text` values are parsed by the engine’s JSON backend (JsonDoc for wasm)
  into node references before evaluation.

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
engine.registerSchema({ name: "r5", prefix: "FHIR", model, isDefault: true });
// Or fetch the model bytes directly:
await engine.registerSchemaFromUrl({ name: "r5", prefix: "FHIR", url: "./model-r5.bin", isDefault: true });

const res = engine.eval({ expr, json, schema: "r5", env });
for (const node of res) {
  console.log(node.meta.typeName);
  // data is lazy, only decoded if accessed
  if (needValue) console.log(node.data);
}

// If you need deterministic now()/today()/timeOfDay(), set the time explicitly:
engine.setNowEpochSeconds(0);
// or per-eval:
engine.eval({ expr: "now()", json, schema: "r5", now: new Date() });
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

    if (kind === 1) { // node_ref
      const ptr = this.engine._itemDataPtr(this.item);
      const len = this.engine._itemDataLen(this.item);
      if (len === 0) {
        this._data = null;
        return this._data;
      }
      const bytes = this.engine._mem.subarray(ptr, ptr + len);
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

`_typeNameFromId(id)` is a thin wrapper over the ABI functions
`fhirpath_type_name_ptr/len`. It resolves `System.*` or schema-qualified
names (e.g., `FHIR.Patient`) using the schema active in the most recent eval.

Note: the `node_ref` path assumes the adapter can provide JSON text (via span
or stringify). If it cannot, `data_len` will be `0` and the wrapper should
fallback to a host-specific materialization path or throw.

### Memory layout diagrams (conceptual)

Input-backed node (JSON text optional, zero-copy when available):
```
WASM memory
+---------------------------------------------------------+
| input JSON buffer ... "dateTime":"2020-01-01T..."      |
+----------------------------^----------------------------+
                             |
Item.data_kind = node_ref    |
item_data_ptr = ptr ---------+   (0 if adapter has no text)
item_data_len = len              (0 if adapter has no text)
Item.source_pos/source_end = span (optional)
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

Input-backed node:
- Engine returns DataKind.node_ref for node-backed results.
- If the adapter provides JSON text, data_ptr/data_len reference the input buffer
  or an engine-owned scratch slice.
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

Type ids **do not** encode schema identity; the schema is selected at eval time.
This allows multiple schemas (FHIR versions or non-FHIR) to coexist in memory.

No FHIR-specific enums are compiled into the engine. All FHIR type resolution
comes from the model blob registered at runtime.

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

---

## NodeAdapter Pattern (JSON Backend Abstraction)

The engine uses a compile-time generic NodeAdapter pattern to abstract JSON
document traversal. This enables:

1. **Multiple backends**: JsonDoc (custom, fast) or std.json.Value (standard library)
2. **Zero-cost abstraction**: Zig monomorphizes all adapter calls at compile time
3. **Optional capabilities**: Span preservation, zero-copy stringify
4. **Future extensibility**: WASM host adapter for direct JS object navigation

### Interface (`src/node.zig`)

Any adapter must implement:

```zig
pub const NodeRef = /* backend-specific node reference type */;

pub fn kind(self: *Adapter, ref: NodeRef) node.Kind;
pub fn objectGet(self: *Adapter, ref: NodeRef, key: []const u8) ?NodeRef;
pub fn objectCount(self: *Adapter, ref: NodeRef) usize;
pub fn objectIter(self: *Adapter, ref: NodeRef) ObjectIter;
pub fn arrayLen(self: *Adapter, ref: NodeRef) usize;
pub fn arrayAt(self: *Adapter, ref: NodeRef, idx: usize) NodeRef;
pub fn string(self: *Adapter, ref: NodeRef) []const u8;
pub fn numberText(self: *Adapter, ref: NodeRef) []const u8;
pub fn boolean(self: *Adapter, ref: NodeRef) bool;
```

Optional capabilities (detected via `@hasDecl`):
```zig
pub fn span(self: *Adapter, ref: NodeRef) node.Span;      // zero-copy source positions
pub fn stringify(self: *Adapter, ref: NodeRef) []const u8; // original JSON text
```

Spans are optional metadata. When an adapter supports `span()`, the evaluator
records the span into the item (`source_pos/source_end`) for debugging and trace
metadata. JSON text access for `node.data` uses either `stringify()` or an
adapter-provided span-backed slice.

Example uses:
- zero-copy slicing of the original JSON text for `node.data`
- trace/debug output that references source offsets
- host-side extraction without materializing full JSON values

Pure Zig usage can bypass JSON serialization entirely: iterate results, keep the
`node_ref` handle, and call adapter methods directly for traversal or scalar reads.
Only `item_data_ptr/len` (or JS `node.data`) triggers JSON materialization.

### Implementations

**JsonDocAdapter** (`src/backends/jsondoc.zig`):
- Wraps our custom `jsondoc.JsonDoc` DOM
- Linear field scan (fast for small objects typical in FHIR)
- Supports `span()` and `stringify()` for zero-copy operations
- Tuned for FHIR-shaped JSON and span preservation

**StdJsonAdapter** (`src/backends/stdjson.zig`):
- Wraps `std.json.Value` from the standard library
- Hash-based object lookup
- Useful when JSON is already parsed elsewhere
- No span support (std.json doesn't preserve source positions)

**FhirJsonAdapter** (`src/backends/fhirjson.zig`):
- Wraps `std.json.Value` with FHIR-aware merging of split primitives
- Merges `field` + `_field` into logical FHIR nodes
- Hides `resourceType` from iteration (accessible via objectGet)
- Hides `_*` keys from both iteration and direct access
- Arena-based virtual nodes for merged primitives and merged arrays
- Optional `*Schema` for type assignment (via evaluator's `childTypeForField`)
- No span support

### Performance and tradeoffs (high-level)

We keep the adapter abstraction so we can choose the backend based on tradeoffs:
- JsonDoc preserves spans for zero-copy output and is tuned for FHIR-shaped JSON.
- StdJson is convenient when JSON is already parsed or when std.json compatibility matters.

The benchmark harness in `src/bench.zig` compares backends under both
parse-once (navigation-only) and parse-each (parse+eval) modes. We avoid
hardcoding specific numbers here because they vary by machine and inputs.

### Usage

The adapter interface is used for benchmark coverage and adapter verification.
The core evaluator is adapter-based, so additional backends can plug in without
duplicating evaluation logic.

Entry points (subject to change):

```zig
pub fn EvalContext(comptime A: type) type {
    return struct {
        allocator: std.mem.Allocator,
        adapter: *A,
        types: *item.TypeTable,
        schema: ?*schema.Schema,
    };
}

// Entry points
pub fn evalWithJson(expr: []const u8, json: []const u8, ...) !Result; // uses JsonDoc adapter
// Optional convenience entry point:
pub fn evalWithStdJson(expr: []const u8, value: *std.json.Value, ...) !Result;
```

### Recommendation

Use JsonDoc by default when you want span preservation or zero-copy outputs.
Use StdJsonAdapter when the input is already a `std.json.Value` or when host
compatibility is more important than span support.
Use FhirJsonAdapter when evaluating FHIR JSON and you need spec-correct
handling of primitive extensions (`_field` merging) and output type conversion.

### Adapter Architecture

Adapters are compiled-in (Zig comptime generics) but selected at runtime.
Schemas are orthogonal and runtime-loaded.

| Adapter | Schema? | Format-aware? | Use case |
|---|---|---|---|
| `StdJsonAdapter` | No | No | Generic JSON querying, no FHIR awareness |
| `JsonDocAdapter` | No | No | Generic JSON with span preservation |
| `FhirJsonAdapter` | Optional | Yes (FHIR JSON) | FHIR JSON logical model |

The same `FhirJsonAdapter` works for any FHIR version; the schema binary
(R4, R5, etc.) is what differs. Adapter + schema are independent choices.

### FHIR Primitive Split Representation

FHIR JSON uses a split representation for primitives:
- `"gender": "male"` — the value
- `"_gender": {"id": "abc", "extension": [...]}` — metadata

The `FhirJsonAdapter` merges these into a single logical node. Per the
FHIRPath spec: "specific xml or json features are not visible to the
FHIRPath language (such as comments and the split representation of
primitives)."

The merged primitive node:
- Reports `kind()` as the value's primitive kind (`.string`, `.number`, etc.)
  for backward compat with implicit value extraction
- Responds to `objectGet("extension")`, `objectGet("id")`,
  `objectGet("value")` to expose the FHIR Element children
- Is hidden from `objectIter` output for `_*` keys and `resourceType`
- Does NOT appear as children in `children()` or `descendants()` — the
  extension/id/value sub-fields are only navigable via explicit property access

### Output Type Conversion

During evaluation, items keep their FHIR types (FHIR.code, FHIR.date, etc.)
so that `.extension`, `.id`, `.value` navigation works on primitives, and
`is()`/`as()` operate on exact FHIR types per the spec.

When returning results to callers (harness with `--fhir-json`, WASM, JS),
FHIR primitive types are downcast to their System equivalents via
`schema.implicitSystemTypeId()`. The mapping is data-driven: the model blob's
`prim_base_id` field stores the System type ID for each FHIR primitive type,
computed at build time by `build_model.zig`. This applies only to FHIR
primitives that have an implicit System type mapping (the table from
fhirpath-in-fhir.html). Complex types (Patient, HumanName, etc.) keep their
FHIR type names.

This affects:
- `harness.zig:fhirItemsToJsonArray()` — type_name resolution for FHIR adapter output
- `wasm.zig:typeNameSlice()` — type name for `fhirpath_type_name_ptr/len`
- `wasm.zig:fhirpath_item_type_id()` — the type_id itself
- JS wrapper `meta.typeName` — reflects the converted type
