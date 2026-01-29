#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TESTS_DIR="$ROOT_DIR/tests"
TMP_DIR="$ROOT_DIR/build/tmp-tests"

REFRESH=0
REPO_ZIP_URL="https://github.com/FHIR/fhir-test-cases/releases/latest/download/testcases.zip"

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

if [[ -f "$TESTS_DIR/r5/tests-fhir-r5.xml" && "$REFRESH" -eq 0 ]]; then
  echo "Using existing tests at $TESTS_DIR/r5"
  exit 0
fi

rm -rf "$TMP_DIR"
mkdir -p "$TMP_DIR"

ZIP_PATH="$TMP_DIR/fhirpath.zip"
echo "Downloading: $REPO_ZIP_URL"
curl -L "$REPO_ZIP_URL" -o "$ZIP_PATH"

echo "Extracting tests"
unzip -q "$ZIP_PATH" -d "$TMP_DIR"

TEST_XML="$(find "$TMP_DIR" -path '*/r5/fhirpath/tests-fhir-r5.xml' -o -name 'tests-fhir-r5.xml' | head -n 1)"
if [[ -z "$TEST_XML" ]]; then
  echo "tests-fhir-r5.xml not found in repo" >&2
  exit 1
fi

mkdir -p "$TESTS_DIR/r5/input"
cp "$TEST_XML" "$TESTS_DIR/r5/tests-fhir-r5.xml"

REPO_ROOT="$TMP_DIR" TESTS_DIR="$TESTS_DIR" python - <<'PY'
import os
import re
import shutil
from pathlib import Path

repo_root = Path(os.environ["REPO_ROOT"]).resolve()
tests_dir = Path(os.environ["TESTS_DIR"]).resolve() / "r5"
tests_xml = tests_dir / "tests-fhir-r5.xml"

text = tests_xml.read_text(encoding="utf-8")
files = set(re.findall(r'inputfile=\"([^\"]+)\"', text))

# Collect both JSON and XML variants of each input file
needed_json = set()
needed_xml = set()
for name in files:
    if name.endswith(".xml"):
        needed_json.add(name[:-4] + ".json")
        needed_xml.add(name)
    elif name.endswith(".json"):
        needed_json.add(name)
        needed_xml.add(name[:-5] + ".xml")
    else:
        needed_json.add(name)

def find_file(name: str) -> Path | None:
    for path in repo_root.rglob(name):
        if path.is_file():
            return path
    return None

missing_json = []
for name in sorted(needed_json):
    src = find_file(name)
    if not src:
        missing_json.append(name)
        continue
    dest = tests_dir / "input" / name
    dest.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(src, dest)

missing_xml = []
for name in sorted(needed_xml):
    src = find_file(name)
    if not src:
        missing_xml.append(name)
        continue
    dest = tests_dir / "input" / name
    dest.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(src, dest)

if missing_json:
    print("Missing input JSON files:")
    for name in missing_json:
        print(f"  - {name}")
if missing_xml:
    print("Missing input XML files:")
    for name in missing_xml:
        print(f"  - {name}")
PY

echo "Tests ready at $TESTS_DIR/r5"
