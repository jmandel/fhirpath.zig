# Artisanal Tests

Hand-authored tests for validating core FHIRPath semantics.

See `methodology.md` in the project root for:
- Complete test format specification
- Field reference (input, expect, skip, invalid, etc.)
- Expected value format
- Spec documentation guidelines

## Quick start

```bash
# Run all artisinal tests
zig build harness

# Run a specific file
zig build harness -- tests/artisinal/string-matching.json

# Filter by pattern
zig build harness -- -f substring

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
