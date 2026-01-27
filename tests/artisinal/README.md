# Artisanal Tests

Hand-authored tests for validating core FHIRPath semantics.

See `methodology.md` in the project root for:
- Complete test format specification
- Field reference (input, expect, skip, expect_error, etc.)
- Expected value format
- Spec documentation guidelines

## Quick start

```bash
# Run all artisinal tests
zig build harness

# Filter by filename
zig build harness -- -F string

# Filter by test name
zig build harness -- -t substring

# Combine filters
zig build harness -- -F math -t divide

# Run official tests
zig build harness -- -m models/r5/model.bin -i tests/r5/input tests/r5/tests-fhir-r5.json
```

## File structure

```json
{
  "meta": {"status": "drafted"},
  "_spec_summary": "Thorough explanation of spec behavior...",
  "_todo": ["[ ] Add edge case tests"],
  "cases": [
    {"name": "test name", "expr": "...", "input": {...}, "expect": [...]}
  ]
}
```
