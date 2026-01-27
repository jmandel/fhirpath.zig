# Artisanal Tests

These tests are hand-authored and meant to validate core semantics early.

Format:
```json
{
  "cases": [
    {
      "name": "simple path",
      "expr": "name.given",
      "input": { "name": { "given": ["Ann"] } },
      "env": { "root": { ... } },
      "expect": ["Ann"]
    }
  ]
}
```

Notes:
- `env` entries use bare names (no `%`).
- `expect` is an array of JSON values.
