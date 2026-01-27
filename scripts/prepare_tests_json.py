#!/usr/bin/env python3
"""Prepare JSON test data from FHIRPath XML test files."""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

try:
    from lxml import etree
except Exception as exc:
    etree = None
    _LXML_ERROR = exc

ROOT = Path(__file__).resolve().parent.parent
DEFAULT_OUT_DIR = ROOT / "tests"


def candidate_tests_dirs() -> list[Path]:
    """Search tests under the current repo root only."""
    return [DEFAULT_OUT_DIR]


def find_tests_dir(version: str) -> Path | None:
    for base in candidate_tests_dirs():
        path = base / version
        if path.exists():
            return path
    return None


def find_tests_xml(version: str) -> Path | None:
    for base in candidate_tests_dirs():
        path = base / version / f"tests-fhir-{version}.xml"
        if path.exists():
            return path
    return None


def find_additional_xml() -> Path | None:
    for base in candidate_tests_dirs():
        path = base / "additional-tests.xml"
        if path.exists():
            return path
    return None


def normalize_output(out_type: str, value: str) -> dict:
    """Normalize output values - strip FHIRPath literal prefixes since type is explicit."""
    result = {"type": out_type}
    
    # Date/DateTime/Time: strip @ prefix (and @T for time)
    if out_type in ("date", "dateTime"):
        if value.startswith("@"):
            value = value[1:]
        result["value"] = value
    elif out_type == "time":
        if value.startswith("@T"):
            value = value[2:]
        elif value.startswith("@"):
            value = value[1:]
        result["value"] = value
    # Quantity: parse "number 'unit'" into structured form
    elif out_type == "Quantity":
        import re
        match = re.match(r"^([\d.+-eE]+)\s*'([^']*)'$", value)
        if match:
            num_str = match.group(1)
            # Convert to number (int or float)
            if '.' in num_str or 'e' in num_str.lower():
                num_val = float(num_str)
            else:
                num_val = int(num_str)
            result["value"] = {
                "value": num_val,
                "unit": match.group(2),
            }
        else:
            # Fallback: keep as string if can't parse
            result["value"] = value
    else:
        result["value"] = value
    
    return result


def parse_tests_xml(path: Path):
    raw = "\n".join(ln for ln in path.read_text().splitlines() if not ln.strip().startswith("<?xml"))
    raw = re.sub(r"<!--.*?-->", "", raw, flags=re.DOTALL)

    def escape_expression_blocks(text: str) -> str:
        out = []
        i = 0
        while True:
            start = text.find("<expression", i)
            if start == -1:
                out.append(text[i:])
                break
            out.append(text[i:start])
            tag_end = text.find(">", start)
            if tag_end == -1:
                out.append(text[start:])
                break
            out.append(text[start:tag_end + 1])
            end_tag = text.find("</expression>", tag_end)
            if end_tag == -1:
                out.append(text[tag_end + 1:])
                break
            content = text[tag_end + 1:end_tag]
            content = re.sub(r"&(?!(?:[a-zA-Z]+|#\\d+);)", "&amp;", content)
            content = content.replace("<", "&lt;")
            out.append(content)
            out.append("</expression>")
            i = end_tag + len("</expression>")
        return "".join(out)

    escaped = escape_expression_blocks(raw)
    if etree is None:
        import html
        tests = []

        def parse_attrs(attr_text: str):
            return {k: v for k, v in re.findall(r'([A-Za-z0-9_-]+)=\"([^\"]*)\"', attr_text)}

        def strip_tags(text: str) -> str:
            return re.sub(r"<[^>]+>", "", text)

        for tag in ("test", "modeTest"):
            if tag == "test":
                pattern = r"<test(?!s)([^>]*)>(.*?)</test>"
            else:
                pattern = r"<modeTest([^>]*)>(.*?)</modeTest>"
            for match in re.finditer(pattern, escaped, flags=re.DOTALL):
                attrs = parse_attrs(match.group(1))
                body = match.group(2)
                expr_match = re.search(r"<expression([^>]*)>(.*?)</expression>", body, flags=re.DOTALL)
                if not expr_match:
                    continue
                expr_attrs = parse_attrs(expr_match.group(1))
                expr_text = html.unescape(strip_tags(expr_match.group(2))).strip()
                if not expr_text:
                    continue
                entry = {
                    "name": attrs.get("name", ""),
                    "expression": expr_text,
                    "inputfile": attrs.get("inputfile", ""),
                    "envfile": attrs.get("envfile", ""),
                }
                if attrs.get("mode"):
                    entry["mode"] = attrs.get("mode", "")
                predicate_attr = attrs.get("predicate")
                if predicate_attr is not None:
                    entry["predicate"] = str(predicate_attr).lower() == "true"
                invalid_attr = expr_attrs.get("invalid") or attrs.get("invalid")
                if invalid_attr is not None and str(invalid_attr).lower() != "false":
                    entry["invalid"] = True
                expected_status = expr_attrs.get("expectedStatus") or attrs.get("expectedStatus")
                if expected_status not in (None, ""):
                    entry["expectedStatus"] = expected_status
                expected_error = expr_attrs.get("expectedErrorCode") or attrs.get("expectedErrorCode")
                if expected_error not in (None, ""):
                    entry["expectedErrorCode"] = expected_error
                outputs = []
                for out_match in re.finditer(r"<output([^>]*)>(.*?)</output>", body, flags=re.DOTALL):
                    out_attrs = parse_attrs(out_match.group(1))
                    value = html.unescape(strip_tags(out_match.group(2))).strip()
                    out_type = out_attrs.get("type", "")
                    outputs.append(normalize_output(out_type, value))
                if outputs:
                    entry["outputs"] = outputs
                tests.append(entry)
        return tests

    parser = etree.XMLParser(recover=True)
    root = etree.fromstring(escaped.encode(), parser=parser)

    tests = []
    for test in root.iter():
        if test.tag not in ("test", "modeTest"):
            continue
        expr_elem = test.find("expression")
        if expr_elem is None:
            continue
        expr_text = "".join(expr_elem.itertext()).strip()
        if not expr_text:
            continue

        entry = {
            "name": test.get("name", ""),
            "expression": expr_text,
            "inputfile": test.get("inputfile", ""),
            "envfile": test.get("envfile", ""),
        }
        if test.get("mode"):
            entry["mode"] = test.get("mode", "")
        predicate_attr = test.get("predicate")
        if predicate_attr is not None:
            entry["predicate"] = predicate_attr.lower() == "true"
        invalid_attr = expr_elem.get("invalid") or test.get("invalid")
        if invalid_attr is not None and str(invalid_attr).lower() != "false":
            entry["invalid"] = True
        expected_status = expr_elem.get("expectedStatus") or test.get("expectedStatus")
        if expected_status not in (None, ""):
            entry["expectedStatus"] = expected_status
        expected_error = expr_elem.get("expectedErrorCode") or test.get("expectedErrorCode")
        if expected_error not in (None, ""):
            entry["expectedErrorCode"] = expected_error

        outputs = []
        for out in test.findall("output"):
            value = "".join(out.itertext()).strip()
            out_type = out.get("type", "")
            outputs.append(normalize_output(out_type, value))
        if outputs:
            entry["outputs"] = outputs

        tests.append(entry)
    return tests


def write_tests_json(out_path: Path, source_path: Path, tests: list[dict]):
    out_path.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "source": str(source_path),
        "tests": tests,
    }
    out_path.write_text(json.dumps(payload, indent=2, ensure_ascii=True))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fhir-version", action="append", help="FHIR version (r4, r5, ...)")
    parser.add_argument("--xml", help="Input XML path (single file)")
    parser.add_argument("--out", help="Output JSON path (single file)")
    parser.add_argument(
        "--out-dir",
        default=str(DEFAULT_OUT_DIR),
        help="Directory to write versioned JSON outputs (default: tests)",
    )
    parser.add_argument("--no-additional", action="store_true", help="Skip additional-tests.xml")
    args = parser.parse_args()

    if args.xml:
        if not args.out:
            print("ERROR: --out is required when using --xml", file=sys.stderr)
            return 2
        xml_path = Path(args.xml)
        if not xml_path.exists():
            print(f"ERROR: {xml_path} not found", file=sys.stderr)
            return 2
        tests = parse_tests_xml(xml_path)
        write_tests_json(Path(args.out), xml_path, tests)
        print(f"Wrote {args.out} ({len(tests)} tests)")
        return 0

    out_dir = Path(args.out_dir)
    versions: list[str] = []
    if args.fhir_version:
        versions = args.fhir_version
    else:
        versions = ["r5"]

    for version in versions:
        version = version.lower().strip()
        xml_path = find_tests_xml(version)
        if not xml_path:
            print(f"ERROR: tests-fhir-{version}.xml not found under tests/", file=sys.stderr)
            return 2
        tests = parse_tests_xml(xml_path)
        out_path = out_dir / version / f"tests-fhir-{version}.json"
        write_tests_json(out_path, xml_path, tests)
        print(f"Wrote {out_path} ({len(tests)} tests)")

    if not args.no_additional:
        additional_xml = find_additional_xml()
        if additional_xml:
            tests = parse_tests_xml(additional_xml)
            out_path = out_dir / "additional-tests.json"
            write_tests_json(out_path, additional_xml, tests)
            print(f"Wrote {out_path} ({len(tests)} tests)")
        else:
            print("INFO: additional-tests.xml not found under tests/, skipping", file=sys.stderr)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
