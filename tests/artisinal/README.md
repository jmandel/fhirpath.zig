# Artisanal Tests

These tests are hand-authored and meant to validate core semantics early.

Format:
```json
{
  "meta": {
    "status": "drafted"
  },
  "cases": [
    {
      "name": "simple path",
      "expr": "name.given",
      "input": { "name": { "given": ["Ann"] } },
      "env": { "root": { ... } },
      "expect": [{"type": "string", "value": "Ann"}]
    }
  ]
}
```

## Expected value format

Each item in `expect` must have explicit `type` and `value` fields:

| Type | Value format | Example |
|------|--------------|--------|
| `string` | string | `{"type": "string", "value": "hello"}` |
| `integer` | string (numeric) | `{"type": "integer", "value": "42"}` |
| `decimal` | string (numeric) | `{"type": "decimal", "value": "3.14"}` |
| `boolean` | string | `{"type": "boolean", "value": "true"}` |
| `date` | string (no @ prefix) | `{"type": "date", "value": "2024-01-15"}` |
| `dateTime` | string (no @ prefix) | `{"type": "dateTime", "value": "2024-01-15T10:30:00Z"}` |
| `time` | string (no @T prefix) | `{"type": "time", "value": "10:30:00"}` |
| `Quantity` | object with numeric value+unit | `{"type": "Quantity", "value": {"value": 10, "unit": "mg"}}` |

**Important**: Do NOT use FHIRPath literal prefixes like `@` for dates or `@T` for times in expected values. The `type` field makes these redundant.

## Running tests

```bash
# Run all artisinal tests
zig build harness

# Run a specific file
zig build harness -- tests/artisinal/string-matching.json

# Filter by pattern (matches file or test names)
zig build harness -- -f substring

# Show only summary (no failure details)
zig build harness -- -q

# Limit failures shown
zig build harness -- -n 10
```

Output shows pass/fail counts per file with breakdown by error type (parse, eval, mismatch).

## Notes

- `env` entries use bare names (no `%`); **env should be supported by the harness**, even though it is not wired yet.
- `meta.status` is used by `scripts/choose_mode.py` to bias EXPLORE/DEVELOP/CONFIRM:
  - `drafted`, `reviewed`, `implemented` (see `methodology.md` for details).
