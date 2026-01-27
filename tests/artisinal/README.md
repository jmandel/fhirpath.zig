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
      "expect": ["Ann"]
    }
  ]
}
```

Notes:
- `env` entries use bare names (no `%`); **env should be supported by the harness**, even though it is not wired yet.
- `expect` is an array of JSON values.
- `meta.status` is used by `scripts/choose_mode.py` to bias EXPLORE/DEVELOP/CONFIRM:
  - `drafted`, `reviewed`, `implemented` (see `methodology.md` for details).
