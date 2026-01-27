#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MODELS_DIR="$ROOT_DIR/models"

REFRESH=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --refresh|--force)
      REFRESH=1
      shift
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 1
      ;;
  esac
done

fetch_version() {
  local version="$1"
  local version_upper="${version^^}"
  local version_lower="${version,,}"
  local dest="$MODELS_DIR/$version_lower"
  local base_url="https://hl7.org/fhir/$version_upper"

  if [[ -d "$dest" && "$REFRESH" -eq 0 ]]; then
    echo "Using existing models at $dest"
    return
  fi

  echo "Fetching FHIR $version_upper models into $dest"
  mkdir -p "$dest"

  echo "- Downloading definitions.zip"
  curl -fsSL "$base_url/definitions.json.zip" -o "$dest/definitions.zip"

  echo "- Downloading profiles-resources.json"
  curl -fsSL "$base_url/profiles-resources.json" -o "$dest/profiles-resources.json"

  echo "- Downloading profiles-types.json"
  curl -fsSL "$base_url/profiles-types.json" -o "$dest/profiles-types.json"

  echo "FHIR $version_upper models ready at $dest"
}

fetch_version "r4"
fetch_version "r5"

echo "All models downloaded."
