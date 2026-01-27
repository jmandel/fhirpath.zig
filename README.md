# fhirpath.zig

A FHIRPath engine implementation in Zig, targeting WebAssembly with a JavaScript wrapper interface.

## Build

```bash
zig build
```

## Setup: Models + Schema Blobs

The engine is schema-driven. For FHIR, you download the published
StructureDefinition bundles and build compact `model.bin` blobs.

```bash
# Download FHIR R4/R5 definitions + profiles into models/
./scripts/fetch_models.sh

# Build model.bin blobs from StructureDefinitions
./scripts/build_models.sh
# Or directly via zig:
zig build build-model -- --fhir-version r4
zig build build-model -- --fhir-version r5
```

Optional: refresh official test fixtures from upstream and regenerate JSON:

```bash
./scripts/fetch_tests.sh
python ./scripts/prepare_tests_json.py --fhir-version r5
```

Refresh-only scripts and what they produce:
- `scripts/fetch_spec.sh` → `spec/` (checked into git)
- `scripts/fetch_tests.sh` → `tests/r5/tests-fhir-r5.xml` + `tests/r5/input/*.json` (checked into git)
- `scripts/prepare_tests_json.py` → `tests/r5/tests-fhir-r5.json` (checked into git)
- `scripts/fetch_models.sh` + `scripts/build_models.sh` → `models/**` (not checked in; ignored by git)

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

### Tests (Unit)

```bash
# Run library unit tests
zig build test
```

### Tests (Artisinal)

Run a single artisinal test file (default is `tests/artisinal/basic.json`):

```bash
zig build harness -- tests/artisinal/where.json
```

Run all artisinal tests:

```bash
for f in tests/artisinal/*.json; do
  zig build harness -- "$f" || exit 1
done
```

Run a filtered subset (example: only files matching "where"):

```bash
for f in tests/artisinal/*where*.json; do
  zig build harness -- "$f" || exit 1
done
```

### Tests (Official FHIRPath Suite)

Full suite (defaults to R5 tests + R5 input + `models/r5/model.bin`):

```bash
zig build official-tests
```

Common filters:

```bash
# Limit to N tests
zig build official-tests -- --limit 50

# Filter by mode (if the JSON test entry has "mode")
zig build official-tests -- --mode myMode

# Use a specific model + prefix
zig build official-tests -- --model models/r4/model.bin --prefix FHIR

# Skip schema/model loading (value-only comparison)
zig build official-tests -- --no-model
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
