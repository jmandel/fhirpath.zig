#!/usr/bin/env python3
"""Combine all artisanal test files into a single published JSON test suite."""

import json
import sys
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TESTS_DIR = ROOT / "tests" / "artisinal"


def build_combined(tests_dir: Path) -> dict:
    suites = []
    total_cases = 0

    for path in sorted(tests_dir.glob("*.json")):
        with open(path, encoding="utf-8") as f:
            data = json.load(f)

        suite_name = path.stem
        meta = data.get("meta", {})
        cases = data.get("cases", [])

        # Add suite-name tag to each case
        for case in cases:
            existing_tags = case.get("tags", [])
            if suite_name not in existing_tags:
                case["tags"] = [suite_name] + existing_tags

        total_cases += len(cases)
        suites.append({
            "name": suite_name,
            "file": path.name,
            "meta": meta,
            "cases": cases,
        })

    return {
        "generated": datetime.now(timezone.utc).isoformat(),
        "engine": "fhirpath.zig",
        "source": "https://github.com/jmandel/fhirpath.zig",
        "suite_count": len(suites),
        "case_count": total_cases,
        "suites": suites,
    }


def main() -> int:
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <output.json>", file=sys.stderr)
        return 1

    out_path = Path(sys.argv[1])
    combined = build_combined(TESTS_DIR)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(combined, f, indent=2, ensure_ascii=False)
        f.write("\n")

    print(
        f"Wrote {combined['case_count']} cases across "
        f"{combined['suite_count']} suites to {out_path}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
