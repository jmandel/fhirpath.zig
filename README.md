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

### WebAssembly + JS (wrapper)

Build the wasm module:

```bash
zig build wasm
```

Node example (uses the JS wrapper):

```bash
node examples/wasm_node.mjs
```

Minimal usage from JS:

```js
import { FhirPathEngine } from "./js/fhirpath.js";

const engine = await FhirPathEngine.instantiate("./fhirpath.wasm");
engine.registerSchema({ name: "r5", prefix: "FHIR", model: modelBytes, isDefault: true });

for (const node of engine.eval({ expr: "name.given", json: patientJson, schema: "r5" })) {
  console.log(node.meta.typeName, node.data);
}
```

### Web demo (landing page)

Assemble a static demo bundle (expects `zig build wasm` + `zig build build-model -- --fhir-version r5` to have been run):

```bash
./scripts/build_web.sh dist
python -m http.server --directory dist
```

Open `http://localhost:8000` in a browser.

CI uploads a `web-dist` artifact with the same layout so anyone can download and run
the demo without local builds.

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

## Supported Functions

- `count()`, `empty()`, `exists()`
- `first()`, `last()`, `tail()`, `single()`
- `skip(n)`, `take(n)`
- `where(predicate)`, `select(projection)`
- `distinct()`, `isDistinct()`

## Cross-checking Against Reference Engines

Compare against fhirpath.js (js), Firely .NET (net), and HAPI FHIR (hapi):

```bash
# Evaluate any expression against reference engines
python scripts/eval_engines.py '1 + 2'
python scripts/eval_engines.py 'name.given' '{"name":{"given":["Ann"]}}'

# Select specific engines (hapi is slow ~22s startup)
python scripts/eval_engines.py -e js,net '(-3.5).round()'

# Sample disagreements for adjudication
python scripts/adjudicate.py --sample 5
```

**Optional dependencies** for cross-checking:
- Bun: `cd vendor/fhirpath.js && bun install`
- .NET 8.0: `cd vendor/fhirpath.net/FhirPathRunner && dotnet build`
- Java 11+ / Maven: `cd vendor/fhirpath.hapi && mvn package`

## Documentation

- `design.md` - Architecture and data structures
- `principles.md` - Design principles and coding practices
- `methodology.md` - Development workflow
