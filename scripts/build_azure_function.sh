#!/usr/bin/env bash
# Assemble the Azure Function App by copying the WASM engine, JS wrapper, and
# FHIR model blobs into the azure-function/ directory, then install npm deps.
#
# Run from the repository root after:
#   zig build wasm -Doptimize=ReleaseSmall
#   ./scripts/build_models.sh
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

FUNC_DIR="$ROOT_DIR/azure-function"

# ── Check prerequisites ────────────────────────────────────────────────────

if [[ ! -f "zig-out/bin/fhirpath.wasm" ]]; then
  echo "ERROR: zig-out/bin/fhirpath.wasm not found." >&2
  echo "       Run: zig build wasm -Doptimize=ReleaseSmall" >&2
  exit 1
fi

if [[ ! -f "models/r5/model.bin" ]]; then
  echo "ERROR: models/r5/model.bin not found." >&2
  echo "       Run: ./scripts/build_models.sh" >&2
  exit 1
fi

# ── Copy files ─────────────────────────────────────────────────────────────

echo "Copying JS wrapper…"
cp js/fhirpath.js "$FUNC_DIR/fhirpath.js"

echo "Copying WASM binary…"
cp zig-out/bin/fhirpath.wasm "$FUNC_DIR/fhirpath.wasm"

echo "Copying R5 model…"
cp models/r5/model.bin "$FUNC_DIR/model-r5.bin"

if [[ -f "models/r4/model.bin" ]]; then
  echo "Copying R4 model…"
  cp models/r4/model.bin "$FUNC_DIR/model-r4.bin"
else
  echo "Warning: models/r4/model.bin not found – R4 schema will not be available" >&2
fi

# ── Install npm dependencies ───────────────────────────────────────────────

echo "Installing npm dependencies…"
cd "$FUNC_DIR"
npm install

echo ""
echo "Azure Function App assembled in azure-function/"
echo "Start locally with:  cd azure-function && func start"
