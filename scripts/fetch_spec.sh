#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SPEC_DIR="$ROOT_DIR/spec"
TMP_DIR="$ROOT_DIR/build/tmp-spec"

REFRESH=0
REPO_ZIP_URL="https://github.com/HL7/fhirpath/archive/refs/heads/master.zip"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --refresh|--force)
      REFRESH=1
      shift
      ;;
    --url)
      REPO_ZIP_URL="$2"
      shift 2
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 1
      ;;
  esac
done

if [[ -d "$SPEC_DIR" && "$REFRESH" -eq 0 ]]; then
  echo "Using existing spec at $SPEC_DIR"
  exit 0
fi

echo "Fetching FHIRPath spec into $SPEC_DIR"
rm -rf "$TMP_DIR"
mkdir -p "$TMP_DIR"

ZIP_PATH="$TMP_DIR/fhirpath.zip"

echo "- Downloading: $REPO_ZIP_URL"
curl -L "$REPO_ZIP_URL" -o "$ZIP_PATH"

echo "- Extracting spec files"
unzip -q "$ZIP_PATH" -d "$TMP_DIR"

# Find the extracted repo root (e.g., fhirpath-master or FHIRPath-master)
REPO_ROOT="$(find "$TMP_DIR" -maxdepth 1 -type d \( -name "fhirpath-*" -o -name "FHIRPath-*" \) | head -n 1)"
if [[ -z "$REPO_ROOT" ]]; then
  echo "Could not find extracted fhirpath repo" >&2
  exit 1
fi

# Prefer the input/ pages (md) as the primary spec source
SRC_INPUT_PAGES="$REPO_ROOT/input/pages"
SRC_INPUT_RESOURCES="$REPO_ROOT/input/resources"
SRC_INPUT_IMAGES="$REPO_ROOT/input/images"

# Fallbacks (older or alternate layouts)
SRC_SPEC_DIR="$REPO_ROOT/spec/N1"
if [[ ! -d "$SRC_SPEC_DIR" ]]; then
  SRC_SPEC_DIR="$REPO_ROOT/spec"
fi
if [[ ! -d "$SRC_SPEC_DIR" ]]; then
  SRC_SPEC_DIR="$REPO_ROOT"
fi

mkdir -p "$SPEC_DIR"

# Copy a focused set of files we actually reference during development
# Prefer input/pages (md) if present
if [[ -d "$SRC_INPUT_PAGES" ]]; then
  for f in index.md grammar.md changes.md tests.md; do
    if [[ -f "$SRC_INPUT_PAGES/$f" ]]; then
      cp "$SRC_INPUT_PAGES/$f" "$SPEC_DIR/$f"
    fi
  done
fi

# Grammar source: prefer input/images if present, else fall back to spec/N1
if [[ -f "$SRC_INPUT_IMAGES/fhirpath.g4" ]]; then
  cp "$SRC_INPUT_IMAGES/fhirpath.g4" "$SPEC_DIR/fhirpath.g4"
elif [[ -f "$SRC_SPEC_DIR/fhirpath.g4" ]]; then
  cp "$SRC_SPEC_DIR/fhirpath.g4" "$SPEC_DIR/fhirpath.g4"
fi

# Avoid confusion between legacy adoc and input/pages md
if [[ -f "$SPEC_DIR/index.md" && -f "$SPEC_DIR/index.adoc" ]]; then
  rm -f "$SPEC_DIR/index.adoc"
fi

# Remove optional binary artifacts if present
rm -f "$SPEC_DIR/binary-grammar.json" "$SPEC_DIR/binary-modelinfoschema.json"

echo "Spec ready at $SPEC_DIR"
