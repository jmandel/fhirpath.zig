# Spike Report: Cross-WASM JS Node Navigation Adapter

## Hypothesis

The dominant cost in the current JS WASM path is not FHIRPath evaluation itself, but
JSON parsing inside WASM. When a JS caller already has a parsed JS object (from
`JSON.parse()`, from a FHIR server response, from an in-memory document), we copy
the JSON *text* into WASM linear memory and re-parse it with Zig's `std.json` — a
full allocating parse that is far slower than V8's optimized native `JSON.parse()`.

**If we keep the parsed object on the JS side and let Zig call back into JS for each
tree-navigation operation, we can skip the WASM-side parse entirely and get
significantly faster end-to-end evaluation for JS callers.**

```
Before:  JS string ──copy──> WASM ──std.json.parse──> Zig tree ──eval──> results
                                     (slow: ~1.7ms on 240KB)

After:   JS object ──stays in JS──> WASM imports ──> Zig eval ──> results
         (V8 JSON.parse: ~0.4ms)    (per-node callbacks)
```

## Results

```
Scenario                                           zig-wasm       zig-js
patient-name.given                                    93 us       36 us
patient-name.where(use=official)                      64 us       33 us
patient-telecom-count                                 50 us       19 us
patient-identifier-system                             80 us       30 us
docref-attachment-url-length                          46 us       19 us
capstmt-experimental                                  52 us        8 us
structdef-resourceType                             1.683 ms       11 us
structdef-snapshot-element-count                   1.695 ms       87 us
binary-contentType                                   987 us        8 us
bundle-entry-resourceType-count                      830 us       93 us
bundle-entry-where-count                             847 us      102 us
bundle-complex-query                                 821 us      145 us
────────────────────────────────────────────────────────────────────────
TOTAL (sum of avgs)                                7.251 ms      590 us
```

**12.3x faster overall** with full FHIR-aware traversal. On large resources:

- `binary-contentType` (130KB resource): **987 us -> 8 us** (123x)
- `structdef-resourceType` (240KB resource): **1.68 ms -> 11 us** (153x)
- `bundle-complex-query` (diagnostic bundle): **821 us -> 145 us** (5.7x)

All 12 scenarios produce matching checksums across all four benchmark paths
(zig-wasm, zig-js, fhirpath.js, fhirpath.js+parse).

### Why so much faster?

Two compounding effects:

1. **No full parse.** The old path runs `std.json.parseFromSliceLeaky` over the
   entire input. For a 240KB StructureDefinition that's ~1.7ms of allocation-heavy
   parsing inside WASM (no SIMD, no JIT). The JS adapter does zero parsing.

2. **Lazy traversal.** The JS adapter only materializes nodes the evaluator actually
   visits. `resourceType` on a 240KB resource touches ~3 nodes; the old path parsed
   all ~4000 nodes upfront.


## Architecture

### Data Flow

```
JS caller                          WASM (Zig)
─────────                          ──────────
resource = JSON.parse(text)
  │
  ├── nodeMap.setRoot(resource)
  │   assigns ID=1 to root
  │
  ├── calls fhirpath_eval_js(ctx, expr, rootId=1, schema, ...)
  │                                  │
  │                                  ├── JsAdapter.init(arena, rootId=1, flavor)
  │                                  ├── eval expression...
  │                                  │
  │   ◄── js_node_kind(1) ─────────►│   kind(root) → object
  │   ◄── js_object_get(1,"name")──►│   objectGet(root,"name") → ID=2
  │   ◄── js_object_get(1,"_name")─►│   (FHIR: check for _ companion)
  │   ◄── js_array_len(2) ─────────►│   arrayLen(nameArr) → 1
  │   ...                            │
  │                                  ├── resolveResult → EvalResult
  │   result ◄───────────────────────┤
```

### Node Handle Encoding

The `JsAdapter` uses a split index space within `nh.NodeHandle` (LSB=1 for
index-backed nodes):

```
index < VIRTUAL_OFFSET (1<<24)  →  JS-side node ID
index >= VIRTUAL_OFFSET         →  Zig virtual node (merged_primitive, merged_array,
                                   quantity, typeInfo, scalar)
```

### WASM Import Interface

Nine `extern "env"` functions form the contract between Zig and JS:

| Import | Signature | Purpose |
|--------|-----------|---------|
| `js_node_kind` | `(ref) → u32` | Kind enum: 0=object, 1=array, 2=string, 3=number, 4=boolean, 5=null |
| `js_object_get` | `(ref, key_ptr, key_len) → u32` | Property lookup; returns child ID or 0 |
| `js_object_count` | `(ref) → u32` | Number of own-properties |
| `js_object_keys` | `(ref, out_ptr, max_pairs) → u32` | Bulk-write (key_id, val_id) pairs |
| `js_array_len` | `(ref) → u32` | Array length |
| `js_array_at` | `(ref, idx) → u32` | Array element at index |
| `js_string` | `(ref, out_ptr, max_len) → u32` | Write UTF-8 string into scratch buffer |
| `js_number_text` | `(ref, out_ptr, max_len) → u32` | Write number-as-text into scratch |
| `js_boolean` | `(ref) → u32` | Boolean value (1=true, 0=false) |


## FHIR-Aware Traversal

The `JsAdapter` has a `Flavor` enum (`generic_json` / `fhir_json`) matching
`JsonAdapter`. When `fhir_json` is active (schema is provided), the adapter
performs all three FHIR merging behaviors:

### 1. Primitive value/metadata merging

On `objectGet(ref, "birthDate")` in FHIR mode, the adapter:
1. Calls `js_object_get(ref, "birthDate")` → gets value ID
2. Calls `js_object_get(ref, "_birthDate")` → gets meta ID (via `getMetaId()`)
3. If value is a primitive (string/number/bool/null), creates a `merged_primitive`
   virtual node with `{ .value = jsRef(val_id), .meta = jsRef(meta_id) }`
4. If value is an array, creates a `merged_array` virtual node
5. If value is an object, returns it directly (objects aren't primitives in FHIR)

The `getMetaId()` helper constructs the `_key` name in the scratch buffer and does
a single `js_object_get` call.

### 2. Merged array element access

On `arrayAt(merged_array_ref, idx)`:
1. Calls `js_array_at(values_ref, idx)` → gets value element
2. Calls `js_array_at(metas_ref, idx)` → gets meta element (if metas exist)
3. For primitive elements: returns a `merged_primitive` pairing value + meta
4. For object/array elements: returns the value directly

### 3. Object iteration filtering

`fhirObjectCount` and `fhirObjectIter`:
- Fetch all keys via `js_object_keys`
- Skip `_`-prefixed keys (accessed only through merge mechanism)
- Skip `resourceType` (type discriminator, not navigable in FHIRPath)
- Count/emit extension-only fields (`_key` exists but `key` doesn't)

### Merged primitive navigation

When the evaluator asks for `.value`, `.id`, or `.extension` on a merged primitive:
- `.value` → returns the value ref directly
- `.id` → calls `js_object_get(meta_ref, "id")`
- `.extension` → calls `js_object_get(meta_ref, "extension")`


## Unified `eval()` API

The JS wrapper exposes a single `eval()` method with auto-detection:

```javascript
// Auto-detect: object → JS adapter, string → WASM adapter
engine.eval({ expr: "name.given", input: resource, schema: "r5" })

// Legacy: json param always uses WASM adapter
engine.eval({ expr: "name.given", json: jsonString, schema: "r5" })

// Force adapter: "js" or "wasm"
engine.eval({ expr, input: resource, schema: "r5", adapter: "wasm" })  // serialize + WASM parse
engine.eval({ expr, input: jsonString, schema: "r5", adapter: "js" })  // JSON.parse + JS adapter

// Control FHIR traversal independently (defaults to true)
engine.eval({ expr, input: resource, schema: "r5", fhir: false })  // plain JSON, no merging
```

**Adapter resolution:**
1. If `adapter` is explicitly set, use it
2. If `json` param is used (legacy), default to `"wasm"`
3. If `input` is a string, default to `"wasm"`
4. If `input` is an object, default to `"js"`

**FHIR mode** (`fhir`, default `true`): controls FHIR-aware traversal
(field/`_field` primitive merging, underscore key hiding, `resourceType`
suppression). Independent of the adapter choice — both `"wasm"` and `"js"`
adapters support both modes.

When `adapter: "js"` is used with a string input, the JS wrapper calls
`JSON.parse()` before passing to the adapter. When `adapter: "wasm"` is used
with an object input, the wrapper calls `JSON.stringify()` to produce the text.

The `evalXml()` method remains separate (XML uses a different adapter entirely).


## Files Changed

### `src/backends/js_adapter.zig` (NEW — 791 lines)

Full `NodeAdapter` implementation with `Flavor` support. Key components:

- **VirtualNode union**: `merged_primitive`, `merged_array`, `value_scalar`,
  `value_quantity`, `value_typeinfo` — matching `JsonAdapter`'s pattern
- **FHIR traversal**: `fhirObjectGet`, `fhirObjectCount`, `fhirObjectIter`,
  `mergedPrimitiveGet`, `mergedPrimitiveIter`, `getMergedArrayMeta`
- **Helper methods**: `getMetaId` (builds `_key` in scratch_buf),
  `readJsString`/`readJsStringDupe` (string transfer helpers)
- **64KB scratch buffer** for string transfer across the WASM boundary
- Comptime interface validation: `node.requireAdapter(JsAdapter)`

### `src/wasm.zig` (MODIFIED)

- Added `JsAdapter` import
- Added `fhirpath_eval_js` export with same structure as `fhirpath_eval`
- Passes `fhir_json` / `generic_json` flavor based on explicit `fhir_mode` parameter

### `js/fhirpath.js` (MODIFIED)

- **`JsNodeMap` class** — `Map<number, any>` for interned JS values
- **`createJsAdapterImports(nodeMap)`** — 9 WASM import functions
- **`FhirPathEngine.instantiate()`** — creates nodeMap, merges imports
- **Unified `eval()`** — auto-detects input type, supports `adapter` and `fhir` flags
- **`#evalJs()` / `#evalWasm()`** — private methods for each path
- **`#writeBytes` fix** — handles zero-length allocations gracefully

### `perf/js/bench-compare.mjs` (MODIFIED)

- Uses unified `eval()` API with explicit `adapter` flags
- Four columns: zig-wasm, zig-js, fhirpath.js, fpjs+parse
- Cross-path checksum verification


## Design Notes

### String lifetime

Arena copies are required. The evaluator holds multiple string refs
simultaneously, so each `string()` / `numberText()` call does
`allocator.dupe()` from the scratch buffer into the arena.

### Node ID growth

`JsNodeMap.intern()` is monotonic — no dedup. The FHIR path does extra
interning (2x `js_object_get` per field for `_` companion lookups), but the
map is cleared between evaluations via `setRoot()`.

### Import requirement

The `extern "env"` declarations make the WASM module require all 9 imports at
instantiation. JS always provides them. Zero overhead when not called.

### scratch_buf sizing

64KB. The `getMetaId` helper also uses the scratch buffer (just the first
~256 bytes) to construct `_key` names. This is safe because `getMetaId` is
only called from `fhirObjectGet`, which doesn't hold a reference to scratch
content across the call.

### FHIR overhead

The FHIR-aware adapter adds ~12% overhead compared to generic-JSON mode
(590 us vs ~526 us in the initial spike). This comes from the extra
`js_object_get` calls for `_field` companions. The overhead is negligible
compared to the 12x speedup over WASM-side parsing.

### Zero-length allocation fix

`fhirpath_alloc(0)` returns -1 (as u32: 4294967295), which the JS `#alloc`
didn't detect as failure. Fixed by short-circuiting `#writeBytes` to return
`{ ptr: 0, len: 0 }` for zero-length inputs. This matters when `schema` is
an empty string.
