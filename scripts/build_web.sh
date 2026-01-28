#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

OUT_DIR="${1:-dist}"

mkdir -p "$OUT_DIR"

# Ensure wasm and model are present
if [[ ! -f "zig-out/bin/fhirpath.wasm" ]]; then
  echo "Missing zig-out/bin/fhirpath.wasm. Run: zig build wasm" >&2
  exit 1
fi
if [[ ! -f "models/r5/model.bin" ]]; then
  echo "Missing models/r5/model.bin. Run: zig build build-model -- --fhir-version r5" >&2
  exit 1
fi

cp web/index.html "$OUT_DIR/index.html"
cp web/styles.css "$OUT_DIR/styles.css"
cp web/app.js "$OUT_DIR/app.js"
cp js/fhirpath.js "$OUT_DIR/fhirpath.js"
cp zig-out/bin/fhirpath.wasm "$OUT_DIR/fhirpath.wasm"
cp models/r5/model.bin "$OUT_DIR/model-r5.bin"

echo "Web demo assembled in $OUT_DIR"
