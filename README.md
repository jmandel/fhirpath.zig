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

### Test Harness

The unified test harness runs both artisinal and official R5 tests:

```bash
# Run all artisinal tests (default)
zig build harness

# Run a specific test file
zig build harness -- tests/artisinal/string-matching.json

# Run official R5 tests
zig build harness -- tests/r5/tests-fhir-r5.json

# Filter by test/file name pattern
zig build harness -- -f testSimple
zig build harness -- -f string tests/r5/tests-fhir-r5.json

# Quiet mode (summary only, no failure details)
zig build harness -- -q

# Limit number of failures shown
zig build harness -- -n 10

# With FHIR model for type resolution
zig build harness -- -m models/r5/model.bin tests/r5/tests-fhir-r5.json

# Verbose mode (show passing tests)
zig build harness -- -v
```

The harness auto-detects the test format:
- **Artisinal**: `cases` array with `expr`, `input`, `expect`
- **Official R5**: `tests` array with `expression`, `inputfile`, `outputs`

Output shows pass/fail counts per file and failure breakdown (parse errors, eval errors, mismatches).

### Tests (Official FHIRPath Suite - Legacy Runner)

The legacy `official-tests` runner is still available:

```bash
zig build official-tests

# Limit to N tests
zig build official-tests -- --limit 50

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
