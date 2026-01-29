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
id; the remaining bits are an index into that schema's Type table. The same
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
- `json_text` values are parsed by the engine's JSON backend (JsonDoc for wasm)
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
// All defaults resolve relative to import.meta.url
const engine = await FhirPathEngine.instantiate({
  schemas: [{ name: "r5", isDefault: true }],
});

// Or with explicit bytes (e.g. Node.js):
const engine = await FhirPathEngine.instantiate({
  wasmBytes: fs.readFileSync("fhirpath.wasm"),
  schemas: [{ name: "r5", model: fs.readFileSync("model-r5.bin"), isDefault: true }],
});

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
const engine = await FhirPathEngine.instantiate({
  imports: {
    functions: {
      trace({ args, helper }) {
        const name = helper.firstString(args[1]) ?? "trace";
        const value = args[2] ?? args[0];
        console.log("[trace]", name, helper.toJS(value));
        return { returnArg: 0 };
      },
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

## Result Model: Adapter-Retained Results

### Problem

The current result flow has a mismatch between the WASM ABI (designed for
lazy, adapter-mediated access) and the native Zig API (which eagerly
materializes everything and drops the adapter):

```
evalExpression()             // returns ItemList with live adapter node_refs
    ↓
resolveResult()              // converts ALL node_refs to std.json.Value,
                             // drops adapter, returns adapter-independent EvalResult
    ↓
EvalResult { items, node_values, arena }   // adapter is gone
```

This causes two problems:

1. **Eager materialization waste**: Every node_ref is converted to
   `std.json.Value` even if the caller never inspects it. For a query
   returning 500 nodes where you inspect 3, you paid for all 500.

2. **Lost navigation capability**: After `resolveResult()`, the adapter is
   gone. You can't navigate into a result node's children, access FHIR
   extensions on a primitive, or call `value_resolver.nodeToValue()` to get
   a type-aware system primitive. The rich adapter interface is only
   available during eval, not after.

In practice, CLI and harness work around this by calling `evalExpression()`
directly and keeping the adapter alive alongside the raw item list. But
there's no structured API for this — each consumer manually threads the
adapter through conversion functions.

### Design: `LiveResult(A)`

Keep the adapter alive in the result. Materialization happens when (and if)
the caller asks for it.

```zig
pub fn LiveResult(comptime A: type) type {
    return struct {
        items: []const item.Item,
        adapter: *A,
        types: *item.TypeTable,
        schema: ?*schema.Schema,
        arena: std.heap.ArenaAllocator,

        const Self = @This();

        /// Number of result items.
        pub fn count(self: *const Self) usize {
            return self.items.len;
        }

        /// Get the system-typed Value for item i.
        /// Calls value_resolver.nodeToValue() for node_ref items; returns
        /// stored value for value items. Lazy — no work until called.
        pub fn value(self: *const Self, i: usize) item.Value {
            const it = self.items[i];
            if (it.data_kind == .value) return it.value orelse .{ .empty = {} };
            if (it.data_kind == .node_ref) {
                // NodeHandle = usize = Item.node, no conversion needed
                return value_resolver.nodeToValue(self.adapter, it.node.?, it.type_id, self.schema);
            }
            return .{ .empty = {} };
        }

        /// Get the JSON kind of item i (object, array, string, number, bool, null).
        pub fn kind(self: *const Self, i: usize) node.Kind {
            const it = self.items[i];
            if (it.data_kind != .node_ref) return .null;
            return A.kind(self.adapter, it.node.?);
        }

        /// Navigate into a node_ref item's child by key.
        /// Returns a node handle or null. Only valid for object-kinded items.
        pub fn objectGet(self: *const Self, i: usize, key: []const u8) ?node.NodeHandle {
            const it = self.items[i];
            if (it.data_kind != .node_ref) return null;
            return A.objectGet(self.adapter, it.node.?, key);
        }

        /// Materialize item i to std.json.Value (full recursive conversion).
        /// This is the expensive path — only call when you need the full JSON tree.
        pub fn toJson(self: *const Self, alloc: Allocator, i: usize) !std.json.Value {
            const it = self.items[i];
            return convert.adapterItemToJsonValue(A, alloc, self.adapter, it);
        }

        /// Type name for item i (e.g., "System.String", "FHIR.Patient").
        pub fn typeName(self: *const Self, i: usize) []const u8 {
            const it = self.items[i];
            if (self.schema) |s| {
                if (schema.isModelType(it.type_id)) return s.typeName(it.type_id);
            }
            return self.types.name(it.type_id);
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }
    };
}
```

The adapter struct is moved into the arena at construction time, so the
`LiveResult` is self-contained — `deinit()` frees everything (adapter,
parsed JSON, items, arena).

### Zig usage

```zig
// Evaluate and get a live result
var result = try eval.evalLive(JsonAdapter, allocator, expr, json, &types, schema_ptr);
defer result.deinit();

for (0..result.count()) |i| {
    const name = result.typeName(i);

    // Lazy: only compute what you need
    switch (result.kind(i)) {
        .object => {
            // Navigate into result node — adapter is still live
            if (result.objectGet(i, "extension")) |ext_ref| {
                // ... inspect extensions without full materialization
            }
            // Or materialize the whole thing when needed
            const json_val = try result.toJson(allocator, i);
        },
        else => {
            // Get the closest System primitive (date, string, integer, etc.)
            const val = result.value(i);
            // val is .date, .string, .integer, etc. — type-aware via value_resolver
        },
    }
}
```

### WASM ABI: unchanged

The WASM path continues to resolve results eagerly because the adapter
cannot cross the WASM ABI boundary. The existing `resolveResult()` becomes
a method on `LiveResult` that produces the adapter-independent form:

```zig
/// Detach from adapter, producing an adapter-independent result for WASM.
/// Eagerly resolves all node_refs to std.json.Value.
pub fn resolve(self: *Self) !EvalResult {
    return resolveResult(A, self.adapter, self.items, &self.arena);
}
```

WASM calls `evalLive()` → `resolve()` → stores `EvalResult` as before.
No ABI changes.

### JS wrapper: new navigation methods

The WASM ABI gains a few new exports for node-level navigation. These let
the JS wrapper expose sub-node access without materializing the full JSON
tree:

```zig
// Navigate into a node_ref item's children (new ABI exports)
export fn fhirpath_item_child_ptr(ctx: u32, item: u32, key_ptr: u32, key_len: u32) u32;
export fn fhirpath_item_child_count(ctx: u32, item: u32) u32;
export fn fhirpath_item_child_at(ctx: u32, item: u32, index: u32) u32;

// Get item's system value kind (calls value_resolver.nodeToValue under the hood)
export fn fhirpath_item_system_value_kind(ctx: u32, item: u32) u32;
```

The JS wrapper adds optional navigation on result nodes:

```js
class FhirPathNode {
  constructor(engine, handle) {
    this.engine = engine;
    this.handle = handle;
    this._data = undefined;
    this._meta = undefined;
    this._value = undefined;
  }

  /** Type metadata (cheap — just reads item fields). */
  get meta() {
    if (this._meta) return this._meta;
    const typeId = this.engine._itemTypeId(this.handle);
    this._meta = {
      typeId,
      typeName: this.engine._typeNameFromId(typeId),
      dataKind: this.engine._itemDataKind(this.handle),
    };
    return this._meta;
  }

  /**
   * System-typed scalar value (lazy).
   * Returns the closest System primitive: boolean, integer, decimal,
   * string, date, dateTime, time, or quantity.
   * For complex objects (Patient, HumanName), returns null — use .data instead.
   */
  get value() {
    if (this._value !== undefined) return this._value;
    const kind = this.engine._itemDataKind(this.handle);
    if (kind === 2) { // DataKind.value — already a scalar
      this._value = this.engine._decodeScalarValue(this.handle);
    } else if (kind === 1) { // DataKind.node_ref
      // Ask the engine to convert via value_resolver.nodeToValue()
      const sysKind = this.engine._itemSystemValueKind(this.handle);
      if (sysKind !== 0) { // not empty — it's a primitive
        this._value = this.engine._decodeScalarValue(this.handle);
      } else {
        this._value = null; // complex node, use .data
      }
    } else {
      this._value = null;
    }
    return this._value;
  }

  /**
   * Full JSON materialization (expensive, lazy).
   * Parses the node_ref into a full JS object. Only pay this cost if you
   * need the complete JSON tree (e.g., for display or serialization).
   */
  get data() {
    if (this._data !== undefined) return this._data;
    const kind = this.engine._itemDataKind(this.handle);
    if (kind === 1) { // node_ref
      const ptr = this.engine._itemDataPtr(this.handle);
      const len = this.engine._itemDataLen(this.handle);
      if (len === 0) { this._data = null; return null; }
      const text = this.engine._decoder.decode(this.engine._mem.subarray(ptr, ptr + len));
      this._data = JSON.parse(text);
    } else if (kind === 2) { // value
      this._data = this.engine._decodeScalarValue(this.handle);
    } else {
      this._data = null;
    }
    return this._data;
  }

  /**
   * Navigate into a child field without materializing the full JSON tree.
   * Returns a new FhirPathNode or null.
   *
   *   const ext = node.child("extension");  // no JSON.parse
   *   if (ext) console.log(ext.child("url")?.value);
   */
  child(key) {
    const keyBytes = this.engine._encoder.encode(key);
    this.engine._writeBytes(keyBytes);
    const childHandle = this.engine._itemChildPtr(this.handle, keyBytes);
    if (childHandle === 0) return null;
    return new FhirPathNode(this.engine, childHandle);
  }

  /**
   * Number of child fields (for object nodes) or elements (for array nodes).
   */
  childCount() {
    return this.engine._itemChildCount(this.handle);
  }
}
```

Usage from JS showing the three access tiers:

```js
const result = engine.eval({ expr: "Patient.birthDate", json: patientJson, schema: "r5" });

for (const node of result) {
  // Tier 1: metadata only (no materialization, ~free)
  console.log(node.meta.typeName);  // "System.Date"

  // Tier 2: system-typed value (value_resolver.nodeToValue, cheap)
  console.log(node.value);          // "1974-12-25" (as a date, not a string)

  // Tier 3: navigate sub-structure (no full materialization)
  const ext = node.child("extension");
  if (ext) {
    const url = ext.child(0)?.child("url");
    console.log(url?.value);         // extension URL, still no JSON.parse
  }

  // Tier 4: full JSON materialization (expensive, only when needed)
  console.log(node.data);           // full JS object via JSON.parse
}
```

### What changes, what doesn't

| Aspect | Before | After |
|---|---|---|
| **Zig native callers** | Call `evalExpression()` directly, manually keep adapter alive, no structured API | Call `evalLive()`, get `LiveResult(A)` with `.value()`, `.kind()`, `.objectGet()`, `.toJson()` |
| **WASM ABI** | `resolveResult()` eagerly materializes, stores `EvalResult` | Same — `evalLive()` → `.resolve()` → `EvalResult`. New optional ABI exports for child navigation |
| **JS wrapper `.data`** | Materializes full JSON on access | Unchanged (still lazy JSON.parse) |
| **JS wrapper `.value`** | Same as `.data` | New: returns system-typed scalar via `nodeToValue()`. Cheap for primitives. Null for complex objects |
| **JS wrapper `.child()`** | Not available | New: navigate into node children without materializing full JSON |
| **Memory** | Arena self-contained after resolve | Arena self-contained (adapter moved into arena). Same lifetime model |
| **Performance** | O(n) eager resolve for all items | O(1) per item, pay only for what you access |

### Costs

1. **Comptime generic**: `LiveResult(A)` is parameterized on the adapter type.
   Native Zig callers already know `A` at compile time, so this is
   transparent. WASM resolves to adapter-independent `EvalResult` as before.

2. **Adapter in arena**: The adapter struct is small (`JsonAdapter` holds a
   flavor flag, a virtual node list, and an allocator). Moving it into
   the arena is cheap. The underlying JSON data is already arena-owned.

3. **New ABI exports (optional)**: `fhirpath_item_child_ptr`,
   `fhirpath_item_child_count`, `fhirpath_item_child_at`,
   `fhirpath_item_system_value_kind`. These are additive — existing ABI
   is unchanged. JS wrapper gains `.value` and `.child()` but `.data`
   and `.meta` work exactly as before.

---

## Node Model and Adapter Architecture

This section describes the current adapter and node handling architecture.
It is the result of a four-phase redesign that unified two separate adapters
into a single `JsonAdapter` with flavor-based dispatch, centralized type
conversion in `value_resolver.zig`, and introduced a single node model for
both input and output.

### Background: previous adapter complexity

The system previously had two separate adapters with different `NodeRef` types:

- `StdJsonAdapter.NodeRef = *const std.json.Value` (pointer)
- `FhirJsonAdapter.NodeRef = u32` (index into internal node table)

`Item.node` was `?usize`, so the system needed bridging helpers (`rawFromNodeRef`,
`nodeRefFromRaw`, `adapterNodeRefFromRaw`) to convert between the generic
storage and the adapter-specific handle. This cascaded complexity:

- Consumers had to thread the adapter to interpret node refs.
- Conversion logic was duplicated across CLI, harness, and WASM.
- FHIR semantics were coupled into a separate adapter rather than being a
  reusable view/flavor.
- Output values (`item.Value`) were not nodes, so you couldn't traverse them
  uniformly without materializing.

### Design principle

**Adapters expose nodes; the engine owns semantics.**

Backends do not embed schema/type logic; views/flavors do not embed
output formatting logic; and callers never have to know whether
something is a node or a value.

### NodeHandle (`src/node_handle.zig`)

All backends expose a single node handle type:

```zig
pub const NodeHandle = usize;
```

Convention:

- **Pointer-backed node** (e.g. `*const std.json.Value`): stored directly,
  **LSB = 0** (guaranteed by alignment of normal allocations).
- **Index-backed virtual/synthetic node**: stored as `(index << 1) | 1`,
  **LSB = 1**.

Helpers:

```zig
pub fn isIndex(h: NodeHandle) bool { return (h & 1) == 1; }
pub fn toIndex(h: NodeHandle) usize { return h >> 1; }
pub fn fromIndex(i: usize) NodeHandle { return (i << 1) | 1; }

pub fn fromPtr(comptime T: type, p: *const T) NodeHandle { return @intFromPtr(p); }
pub fn toPtr(comptime T: type, h: NodeHandle) *const T { return @ptrFromInt(h); }
```

Why this is the sweet spot:

- Generic JSON stays pointer-fast (no wrapper allocation per property access).
- FHIR-aware JSON creates virtual nodes only when needed (merged primitives).
- XML can be pointer-backed too, with virtual nodes for array-of-children views.
- Output values can be synthetic nodes without changing the handle type.

Since `NodeHandle = usize = Item.node`, no conversion is needed between the
item's stored node reference and the adapter's handle type.

### Backend vs. Flavor separation

**Backend** = where data comes from:

- JSON parsed to `std.json.Value` (pointer-backed)
- Future: custom JSON DOM (index-backed, with spans)
- Future: XML DOM (pointer-backed or index-backed)

**Flavor/View** = how traversal behaves:

- **Generic JSON** (`.generic_json`): plain object/array traversal, no hiding, no merging.
- **FHIR JSON** (`.fhir_json`): hide `_foo` keys, merge `foo` + `_foo` as logical
  primitive, hide `resourceType` from iteration.
- **FHIR XML** (future): map `<foo value="x"/>` into logical primitive,
  surface `id`/`extension` semantics.
- **Generic XML** (future): repeated child elements become arrays.

Flavor is not a separate adapter type. It is configuration plus a small
amount of virtual node logic within a single adapter implementation.

### Adapter contract (`src/node.zig`)

Required (structural + scalar):

```zig
pub const NodeRef = node.NodeHandle;

pub fn root(self: *A) NodeRef;
pub fn kind(self: *A, ref: NodeRef) Kind;
pub fn objectGet(self: *A, ref: NodeRef, key: []const u8) ?NodeRef;
pub fn objectIter(self: *A, ref: NodeRef) ObjectIter;
pub fn objectCount(self: *A, ref: NodeRef) usize;
pub fn arrayLen(self: *A, ref: NodeRef) usize;
pub fn arrayAt(self: *A, ref: NodeRef, idx: usize) NodeRef;
pub fn string(self: *A, ref: NodeRef) []const u8;
pub fn numberText(self: *A, ref: NodeRef) []const u8;
pub fn boolean(self: *A, ref: NodeRef) bool;
```

Optional capabilities (detected via `@hasDecl`):

```zig
pub fn span(self: *A, ref: NodeRef) ?Span;        // source positioning
pub fn rawSlice(self: *A, ref: NodeRef) ?[]const u8; // exact JSON/XML slice
pub fn stringify(self: *A, ref: NodeRef, alloc: Allocator) ![]const u8; // materialize on demand
```

**Not part of the adapter contract**: `toValue()`. Type conversion is
centralized in `value_resolver.zig` (see below).

Additional adapter methods for synthetic output nodes:

```zig
pub fn nodeFromValue(self: *A, val: item.Value) NodeHandle;  // create synthetic node from Value
pub fn itemNode(self: *A, it: item.Item) NodeHandle;         // get NodeHandle for any item (node_ref or value)
```

### Centralized type conversion (`src/value_resolver.zig`)

A shared module handles all node-to-value conversion:

```zig
// src/value_resolver.zig
pub fn nodeToValue(adapter: anytype, handle: NodeHandle, type_id: u32, schema: ?*Schema) item.Value;
```

This centralizes:

- JSON-kind-based conversion (string/number/boolean to Value).
- Schema-driven implicit system type mapping (e.g., `FHIR.date` to `System.Date`).
- Quantity extraction from FHIR Quantity objects with value/code/unit fields.

The adapter does not need to know about schemas or type semantics.
Call sites use `value_resolver.nodeToValue()` with `ctx.schema`:

- `itemToValue()` in `eval.zig` uses `value_resolver.nodeToValue()` with `ctx.schema`.
- `resolveResult()` takes a `schema_ptr` parameter and uses `value_resolver.nodeToValue()`.

### Schema-driven type flow

When a FHIR schema is loaded, type information flows through three stages:

**1. Property access (eval.zig `applySegment`):**
The evaluator checks `ctx.schema` at each property access. If a schema is
present and the parent item has a model type_id, it calls
`schema.childTypeForField(parent_type_id, field_name)` to get the child's
FHIR type_id (e.g., navigating `Patient.birthDate` yields `FHIR.date`).
This walks the type's inheritance chain (Patient -> DomainResource -> Resource)
to find fields defined at any level. Without a schema, child type_id is 0.

**2. Item creation:**
The child type_id is stored on the resulting `item.Item`. During evaluation,
items carry their FHIR model type (e.g., `FHIR.date`, `FHIR.code`) so that
`is()`, `as()`, and property access on FHIR primitives work correctly.

**3. Value conversion (`value_resolver.nodeToValue()`):**
When the evaluator needs a typed Value (for equivalence, output, etc.), it
calls `value_resolver.nodeToValue(adapter, handle, type_id, schema)`. The
resolver uses the type_id and schema to produce the correct Value variant:

- **With schema**: checks `schema.implicitSystemTypeId(type_id)` to map FHIR
  model types to System types. A JSON string with type_id `FHIR.date` becomes
  `.{ .date = "1990-01-01" }`. A FHIR Quantity object becomes
  `.{ .quantity = .{ .value = "120", .unit = "mmHg" } }`.
- **Without schema**: falls back to JSON-kind-based conversion. The same string
  becomes `.{ .string = "1990-01-01" }` — correct structurally but loses date
  semantics for equivalence and comparison operations.

The schema lives on `EvalContext.schema` only (not on the adapter). The
evaluator uses `ctx.schema` for `childTypeForField()` lookups during property
navigation, for `is()`/`as()`/`ofType()` type checks, and passes it to
`value_resolver.nodeToValue()` for type resolution.

### Single node model for input AND output

Every result item can be treated as a node:

- If an item is node-backed, it already has a NodeHandle.
- If an item is value-backed, `nodeFromValue()` creates a **synthetic node**
  in the adapter's virtual-node table and returns a NodeHandle for it.

Synthetic node types needed:

- Scalar nodes: null/bool/number/string
- Object nodes: `System.Quantity` -> `{ value, unit }`,
  `Reflection.TypeInfo` -> `{ namespace, name }`

This means `kind()`, `objectGet()`, `string()`, `numberText()`, etc. all
work uniformly on any result item.

### Unified JSON adapter (`src/backends/json_adapter.zig`)

One `JsonAdapter` with a flavor flag replaces the previous `StdJsonAdapter` and
`FhirJsonAdapter`:

```zig
pub const Flavor = enum { generic_json, fhir_json };

pub const JsonAdapter = struct {
    flavor: Flavor,
    virtual_nodes: ArrayListUnmanaged(VirtualNode),
    // ...
};
```

The `JsonAdapter` has NO `schema` field; schema lives only on `EvalContext`.

Node dispatch:

- If `!isIndex(ref)`: treat as `*const std.json.Value`, direct pointer access.
- If `isIndex(ref)`: read `virtual_nodes[toIndex(ref)]`, dispatch on variant.

Virtual node union (5 variants):

```zig
const VirtualNode = union(enum) {
    // FHIR JSON view
    merged_primitive: struct { value: ?NodeHandle, meta: ?NodeHandle },
    merged_array: struct { values: NodeHandle, metas: ?NodeHandle },

    // Synthetic output nodes
    value_scalar: item.Value,
    value_quantity: item.Quantity,
    value_typeinfo: item.TypeInfo,
};
```

Flavor switch: `objectGet()` and `objectIter()` behavior changes based on
`.flavor`. Generic JSON does plain traversal; FHIR JSON hides `_foo` keys,
merges split primitives, and hides `resourceType` from iteration.

#### FHIR Primitive Split Representation

FHIR JSON uses a split representation for primitives:
- `"gender": "male"` — the value
- `"_gender": {"id": "abc", "extension": [...]}` — metadata

The `JsonAdapter` with `.fhir_json` flavor merges these into a single logical
node. Per the FHIRPath spec: "specific xml or json features are not visible to
the FHIRPath language (such as comments and the split representation of
primitives)."

The merged primitive node:
- Reports `kind()` as the value's primitive kind (`.string`, `.number`, etc.)
  for backward compat with implicit value extraction
- Responds to `objectGet("extension")`, `objectGet("id")`,
  `objectGet("value")` to expose the FHIR Element children
- Is hidden from `objectIter` output for `_*` keys and `resourceType`
- Does NOT appear as children in `children()` or `descendants()` — the
  extension/id/value sub-fields are only navigable via explicit property access

#### Output Type Conversion

During evaluation, items keep their FHIR types (FHIR.code, FHIR.date, etc.)
so that `.extension`, `.id`, `.value` navigation works on primitives, and
`is()`/`as()` operate on exact FHIR types per the spec.

Type-aware value conversion is handled by `value_resolver.nodeToValue()`.
When the evaluator needs a typed `item.Value` from a node reference — for
equivalence checks, `resolveResult()`, or other operations — it calls
`value_resolver.nodeToValue(adapter, handle, type_id, schema)`. The resolver
uses the type_id (and optionally the schema) to produce the correct Value
variant:

- It checks well-known `SystemTypeIds` to tag strings as `.date`, `.time`, or
  `.dateTime`.
- With a schema, it additionally consults `schema.implicitSystemTypeId()` to
  map FHIR model types (e.g., `FHIR.date` -> `System.Date`) and can extract
  `System.Quantity` from FHIR Quantity objects with value/code/unit fields.

The equivalence functions (`valueEquivalent`, `itemsEquivalent`) no longer
need type_id parameters — they operate purely on the Value tags produced
by `nodeToValue()`.

When returning results to callers (harness with `--fhir-json`, WASM, JS),
FHIR primitive types are downcast to their System equivalents via
`schema.implicitSystemTypeId()`. The mapping is data-driven: the model blob's
`prim_base_id` field stores the System type ID for each FHIR primitive type,
computed at build time by `build_model.zig`. This applies only to FHIR
primitives that have an implicit System type mapping (the table from
fhirpath-in-fhir.html). Complex types (Patient, HumanName, etc.) keep their
FHIR type names.

This affects:
- `harness.zig:itemsToJsonArray()` — type_name resolution
- `wasm.zig:typeNameSlice()` — type name for `fhirpath_type_name_ptr/len`
- `wasm.zig:fhirpath_item_type_id()` — the type_id itself
- JS wrapper `meta.typeName` — reflects the converted type

### How XML fits later

XML adapters use the same virtual node overlay:

- Underlying nodes: pointer handles to XML DOM nodes.
- Virtual nodes:
  - `child_list_array`: the N `<given>` children under `<name>`.
  - `fhir_xml_primitive`: element whose value is in `value=""` attribute.
  - Same synthetic output nodes as JSON.

The virtual node mechanism generalizes naturally to any backend that needs
to present a different shape than the underlying DOM provides.

### Results in this model

**LiveResult**: holds arena + adapter (with virtual-node table) + items.
Exposes `count()`, `node(i)` (returns NodeHandle for both node_ref and
value items), navigation, scalar access, lazy stringify. Value items become
synthetic nodes automatically.

**EvalResult (detached)**: obtained by `live.resolve()`. Materializes JSON
values/text for ABI boundaries. This is a boundary operation, not the
default semantics.

---

## Testing & Debugging Plan (Zig-First)

We will run the full test suite in native Zig first (non-wasm) to maximize
debug visibility, error reporting, and introspection. The wasm boundary should
be crossed only after core semantics are validated in Zig.

Implications:
- Build a Zig-native test harness for spec tests and artisinal tests.
- Provide rich error context (token spans, AST dumps, eval traces) in Zig.
- Mirror those tests in wasm only after behavior is stable.

The test harness (`src/harness.zig`) uses `JsonAdapter` with `.generic_json`
flavor by default and `.fhir_json` flavor when `--fhir-json` is specified.
It auto-detects the FHIR model for test files under `tests/r5/` or
`tests/r4/`, loading `models/<version>/model.bin` automatically (falling back
silently if not found). An explicit `-m` / `--model` flag overrides
auto-detection.

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

Field entries are sorted by `name` within each type's range when possible, but
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
1) Get type's `field_start/field_count`.
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

## Part 2: Engine Internals

### Well-Known System Type IDs (`item.SystemTypeIds`)

The `TypeTable` initializes System types in a deterministic order, producing
well-known type IDs that the evaluator and value resolver can reference without
hash lookups:

```zig
pub const SystemTypeIds = struct {
    pub const any = 1;
    pub const boolean = 2;
    pub const integer = 3;
    pub const long = 4;
    pub const decimal = 5;
    pub const string = 6;
    pub const date = 7;
    pub const dateTime = 8;
    pub const time = 9;
    pub const quantity = 10;
};
```

These constants are used by `value_resolver.nodeToValue()` to decide
whether a JSON string node should be tagged as `.date`, `.time`, or
`.dateTime` rather than `.string`, avoiding runtime type-name lookups.

### Comptime Enum Function Dispatch

The evaluator resolves FHIRPath function names via a comptime `FnTag` enum
and `std.meta.stringToEnum`. This replaces ~107 sequential `std.mem.eql`
comparisons with a perfect-hash O(1) lookup:

```zig
const FnTag = enum {
    now, today, timeOfDay, count, empty, exists,
    where, select, first, last, tail, skip, take,
    // ... ~60 more function names
};

fn evalFunction(ctx, call, input, env) !ItemList {
    const tag = std.meta.stringToEnum(FnTag, call.name) orelse return error.InvalidFunction;
    switch (tag) { ... }
}
```

The enum values match FHIRPath function names exactly (using `@"is"`,
`@"as"`, `@"union"`, `@"type"` for Zig keywords). Unknown function names
fail immediately with `InvalidFunction`.

### Benchmarks

Two benchmark suites exist:
- `src/bench.zig` — adapter-level navigation benchmarks (parse, traverse, field lookup)
- `src/bench_eval.zig` — end-to-end evaluation benchmarks (JSON parse + TypeTable init + expression eval)

The eval benchmarks cover property access, function calls, filtering,
literal expressions, and bundle traversal at various scales.

### Remaining topics (to be expanded):
- Memory/arena model and lifetime.
- AST, value, and collection representations.
- Schema/model access and type inference.
- Custom function invocation strategy.
- Error handling and tracing.

