# Zig Engine Principles & Practices

This document describes design principles and coding practices for the greenfield
Zig FHIRPath engine. It is intentionally independent of the legacy WAT codebase.

## Principles

1) Correctness First
   - The spec is the source of truth; tests are evidence.
   - Make scope, precision, and equivalence semantics explicit.

2) Deterministic Semantics
   - Same inputs yield the same outputs.
   - Undefined order stays undefined and is detectable.

3) Zero-Copy by Default
   - Preserve JSON spans unless transformation is required.
   - Lazy parse numeric/string values only when needed.

4) Preserve Precision
   - Decimal keeps both `scale` and `norm_scale`.
   - Date/Time/DateTime keep ISO-8601 strings as-is.
   - Quantity is `{ value, unit }` with no hidden normalization.

5) Zig-Idiomatic Internals
   - Use tagged unions and named payloads.
   - Use slices and allocators, not raw offsets.
   - Keep ABI-specific layouts at the boundary only.

6) Clear Ownership & Lifetimes
   - All allocations belong to a `Context`.
   - No value handle survives a `ctx_reset` or `eval`.

7) Structured Errors
   - Return status + error detail; avoid traps except in debug.
   - Errors should be reproducible and diagnosable.

8) Stable, Narrow ABI
   - Keep exported functions minimal and stable.
   - Add new features via versioned options or new exports.

9) Schema-Aware Type System
   - System types (Boolean, Integer, String, etc.) are built-in.
   - FHIR model types are loaded from binary schema blobs.
   - Every Item carries a type_id for full type tracking.
   - Type names use prefix notation: System.String, FHIR.Patient.

10) Lexer/Parser Discipline
   - Single-pass lexer, explicit comment handling.
   - Precedence-driven parser, no hidden backtracking.
   - AST nodes are compact and immutable.

11) Explicit Evaluation Model
   - Eval uses explicit `Context`, `Env`, `Collection`.
   - Per-item functions snapshot/restore env explicitly.

12) Fast by Construction
   - Avoid allocations in hot paths.
   - Use arenas and small fixed-size structs for core ops.

13) Debuggability
   - Optional debug checks (bounds, canaries).
   - Trace is a custom function, not a side channel.

14) Policy-Driven Gray Areas
   - Centralize policy flags for spec ambiguities.

15) Layered Testing
   - Unit tests for decimals, quantity, string ops.
   - Golden tests for lexer/parser.
   - Spec tests for integration.

## Practices (Concrete Guidance)

### Memory & Context
- `Context` owns arenas, model data, caches, error state.
- Arena is reset at `eval` and `ctx_reset`.
- No cross-eval global state.

### Values & Precision
- Use `Value` tagged union internally.
- Decimal payload holds `scale` and `norm_scale`.
- Date/time values are ISO-8601 strings; precision is implicit in the string.

### Lexer
- Single scan loop with explicit state.
- Handle `//` and `/* */` before token logic.
- Keep tokens as `(kind, span)`; no allocation.

### Parser
- Precedence-climbing or Pratt parser.
- AST nodes are small structs with tag + payload.
- Parse errors are positional and stable.

### Eval
- Evaluate into collections of `Item` handles.
- `Item` holds `DataKind` + metadata + data descriptor.
- Use `node_ref` for input-backed values whenever possible.

### Custom Functions
- Name -> host_id lookup table in opts.
- Host receives evaluated args; `argv[0]` is receiver.
- Host returns a `ValueBlob` copied into the engine arena.

### Error Handling
- Functions return status; errors recorded in context.
- Avoid implicit error swallowing; bubble status up.

### Performance
- Keep small structs POD-like.
- Use inline small helper functions instead of macro-like logic.
- Avoid dynamic dispatch in hot loops.

### Debug / Trace
- Debug flags enable extra checks.
- Trace is implemented as custom function for flexibility.
