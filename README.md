# fhirpath.zig

A FHIRPath engine implementation in Zig, targeting WebAssembly with a JavaScript wrapper interface.

## Build

```bash
zig build
```

## Usage

### Ad-hoc CLI

Evaluate a FHIRPath expression against JSON input:

```bash
zig build run -- '<expression>' '<json>'
```

Examples:

```bash
# Path navigation
zig build run -- 'name.given' '{"name":{"given":["Ann","Bob"]}}'
# ["Ann", "Bob"]

# Filtering
zig build run -- "name.where(use = 'official')" '{"name":[{"use":"official","given":["Ann"]}]}'
# [{"use": "official", "given": ["Ann"]}]

# Functions
zig build run -- 'items.count()' '{"items":[1,2,3]}'
# [3]

# Help
zig build run -- --help
```

Note: FHIRPath string literals use single quotes (`'official'`), not double quotes.

### Tests

```bash
# Run library unit tests
zig build test

# Run artisinal test harness (all test files)
zig build harness

# Run a specific test file
zig build harness -- tests/artisinal/where.json
```

## Supported Functions

- `count()`, `empty()`, `exists()`
- `first()`, `last()`, `tail()`, `single()`
- `skip(n)`, `take(n)`
- `where(predicate)`, `select(projection)`
- `distinct()`, `isDistinct()`

## Documentation

- `design.md` - Architecture and data structures
- `principles.md` - Design principles and coding practices
- `methodology.md` - Development workflow
