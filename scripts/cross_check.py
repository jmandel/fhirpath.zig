#!/usr/bin/env python3
"""Cross-check artisinal tests against fhirpath.js reference implementation.

Usage:
  cross_check.py [options] [file_pattern]

Options:
  -v, --verbose      Show all results, not just differences
  -n N               Limit to N tests  
  --sample N         Random sample of N differences (for CROSS_CHECK mode)
  --include-adjudicated  Include already-adjudicated tests
  
Examples:
  cross_check.py                    # Check all unadjudicated tests
  cross_check.py basic              # Check tests matching 'basic'
  cross_check.py --sample 3         # Random sample of 3 diffs for adjudication
"""

import json
import subprocess
import sys
import os
import random
from pathlib import Path

ROOT = Path(__file__).parent.parent
FHIRPATH_JS = ROOT / "vendor" / "fhirpath.js" / "cli.ts"
ARTISINAL_DIR = ROOT / "tests" / "artisinal"

def run_fhirpath_js_batch(tests: list[dict]) -> list[dict]:
    """Run multiple tests through fhirpath.js in one batch call."""
    if not tests:
        return []
    
    batch_input = [{"expr": t["expr"], "input": t.get("input", {})} for t in tests]
    
    try:
        result = subprocess.run(
            ["bun", str(FHIRPATH_JS), "--batch"],
            input=json.dumps(batch_input),
            capture_output=True,
            text=True,
            timeout=60,
            env={**os.environ, "PATH": f"{os.environ['HOME']}/.bun/bin:{os.environ.get('PATH', '')}"}
        )
        if result.returncode != 0:
            # Return errors for all
            return [{"error": result.stderr.strip()}] * len(tests)
        return json.loads(result.stdout)
    except Exception as e:
        return [{"error": str(e)}] * len(tests)

def normalize_type(t: str) -> str:
    """Normalize type names for comparison."""
    t = t.lower()
    mappings = {
        "system.boolean": "boolean",
        "system.integer": "integer", 
        "system.decimal": "decimal",
        "system.string": "string",
        "system.date": "date",
        "system.datetime": "datetime",
        "system.time": "time",
        "system.quantity": "quantity",
    }
    return mappings.get(t, t)

def normalize_value(v):
    """Normalize a value for comparison."""
    if isinstance(v, float):
        rounded = round(v, 10)
        if rounded == int(rounded):
            return int(rounded)
        return rounded
    if isinstance(v, dict):
        return {k: normalize_value(vv) for k, vv in v.items()}
    if isinstance(v, list):
        return [normalize_value(x) for x in v]
    return v

def extract_value(item: dict) -> any:
    """Extract the value from a typed item, normalizing it."""
    if isinstance(item, dict) and "value" in item:
        return normalize_value(item["value"])
    return normalize_value(item)

def types_compatible(our_type: str, js_type: str) -> bool:
    """Check if types are compatible, accounting for JS limitations."""
    our_type = normalize_type(our_type)
    js_type = normalize_type(js_type)
    
    if our_type == js_type:
        return True
    if js_type == "number" and our_type in ("integer", "decimal"):
        return True
    return False

def compare_results(expected: list, actual: list) -> tuple[bool, str]:
    """Compare expected vs actual results. Returns (match, reason)."""
    if len(expected) != len(actual):
        return False, f"length mismatch: expected {len(expected)}, got {len(actual)}"
    
    for i, (e, a) in enumerate(zip(expected, actual)):
        e_type = normalize_type(e.get("type", "unknown") if isinstance(e, dict) else "unknown")
        a_type = normalize_type(a.get("type", "unknown") if isinstance(a, dict) else "unknown")
        
        if not types_compatible(e_type, a_type):
            return False, f"type mismatch at [{i}]: expected {e_type}, got {a_type}"
        
        e_val = extract_value(e)
        a_val = extract_value(a)
        
        if e_val != a_val:
            return False, f"value mismatch at [{i}]: expected {e_val}, got {a_val}"
    
    return True, "match"

def load_all_tests(test_files: list[Path], include_adjudicated: bool = False) -> list[dict]:
    """Load all test cases from files."""
    all_tests = []
    for test_file in test_files:
        with open(test_file) as f:
            data = json.load(f)
        cases = data.get("cases", data.get("tests", []))
        for c in cases:
            if not ("expr" in c or "expression" in c):
                continue
            if not include_adjudicated and "_adjudicated" in c:
                continue
            c["_file"] = test_file.name
            c["expr"] = c.get("expr") or c.get("expression")
            all_tests.append(c)
    return all_tests

def main():
    args = sys.argv[1:]
    verbose = "-v" in args or "--verbose" in args
    include_adjudicated = "--include-adjudicated" in args
    args = [a for a in args if a not in ("-v", "--verbose", "--include-adjudicated")]
    
    limit = None
    sample_size = None
    
    if "-n" in args:
        idx = args.index("-n")
        limit = int(args[idx + 1])
        args = args[:idx] + args[idx+2:]
    
    if "--sample" in args:
        idx = args.index("--sample")
        sample_size = int(args[idx + 1])
        args = args[:idx] + args[idx+2:]
    
    pattern = args[0] if args else ""
    
    # Find test files
    test_files = sorted(ARTISINAL_DIR.glob("*.json"))
    if pattern:
        test_files = [f for f in test_files if pattern in f.name]
    
    # Load all tests
    all_tests = load_all_tests(test_files, include_adjudicated)
    
    if limit:
        all_tests = all_tests[:limit]
    
    # For sampling mode, shuffle and process in batches until we have enough diffs
    if sample_size:
        random.shuffle(all_tests)
        diffs = []
        errors = []
        matches = 0
        batch_size = 50  # Process in batches of 50
        
        for i in range(0, len(all_tests), batch_size):
            batch = all_tests[i:i+batch_size]
            results = run_fhirpath_js_batch(batch)
            
            for test, result in zip(batch, results):
                if "error" in result:
                    errors.append({
                        "file": test["_file"],
                        "name": test.get("name", "unnamed"),
                        "expr": test["expr"],
                        "input": test.get("input", {}),
                        "expected": test.get("expect", []),
                        "error": result["error"]
                    })
                else:
                    expected = test.get("expect", [])
                    actual = result.get("results", [])
                    match, reason = compare_results(expected, actual)
                    
                    if match:
                        matches += 1
                    else:
                        diffs.append({
                            "file": test["_file"],
                            "name": test.get("name", "unnamed"),
                            "expr": test["expr"],
                            "input": test.get("input", {}),
                            "expected": expected,
                            "fhirpath_js": actual,
                            "reason": reason
                        })
            
            # Stop early if we have enough diffs
            if len(diffs) >= sample_size:
                break
        
        # Output sampled diffs
        sampled = diffs[:sample_size]
        if sampled:
            print(f"=== SAMPLED {len(sampled)} DIFFS FOR ADJUDICATION ===\n")
            for d in sampled:
                print(f"[DIFF] {d['file']}:{d['name']}")
                print(f"  expr: {d['expr']}")
                if d['input'] != {}:
                    print(f"  input: {json.dumps(d['input'])}")
                print(f"  our expected: {json.dumps(d['expected'])}")
                print(f"  fhirpath.js:  {json.dumps(d['fhirpath_js'])}")
                print(f"  reason: {d['reason']}")
                print()
        elif errors:
            sampled_errors = errors[:sample_size]
            print(f"=== SAMPLED {len(sampled_errors)} ERRORS FOR REVIEW ===\n")
            for e in sampled_errors:
                print(f"[ERROR] {e['file']}:{e['name']}")
                print(f"  expr: {e['expr']}")
                print(f"  error: {e['error']}")
                print()
        else:
            print("No unadjudicated diffs or errors found!")
        
        total = matches + len(diffs) + len(errors)
        print("=" * 60)
        print(f"Processed: {total}, Matches: {matches}, Diffs: {len(diffs)}, Errors: {len(errors)}")
        return
    
    # Non-sampling mode: run all tests in batches
    results = run_fhirpath_js_batch(all_tests)
    
    diffs = []
    errors = []
    matches = 0
    
    for test, result in zip(all_tests, results):
        if "error" in result:
            errors.append({
                "file": test["_file"],
                "name": test.get("name", "unnamed"),
                "expr": test["expr"],
                "input": test.get("input", {}),
                "expected": test.get("expect", []),
                "error": result["error"]
            })
        else:
            expected = test.get("expect", [])
            actual = result.get("results", [])
            match, reason = compare_results(expected, actual)
            
            if match:
                matches += 1
                if verbose:
                    print(f"[MATCH] {test['_file']}:{test.get('name', 'unnamed')}")
            else:
                diffs.append({
                    "file": test["_file"],
                    "name": test.get("name", "unnamed"),
                    "expr": test["expr"],
                    "input": test.get("input", {}),
                    "expected": expected,
                    "fhirpath_js": actual,
                    "reason": reason
                })
    
    # Output diffs
    if diffs:
        print(f"=== DIFFS ({len(diffs)}) ===\n")
        for d in diffs:
            print(f"[DIFF] {d['file']}:{d['name']}")
            print(f"  expr: {d['expr']}")
            if d['input'] != {}:
                print(f"  input: {json.dumps(d['input'])}")
            print(f"  our expected: {json.dumps(d['expected'])}")
            print(f"  fhirpath.js:  {json.dumps(d['fhirpath_js'])}")
            print(f"  reason: {d['reason']}")
            print()
    
    total = matches + len(diffs) + len(errors)
    print("=" * 60)
    print(f"Total: {total}, Matches: {matches}, Diffs: {len(diffs)}, Errors: {len(errors)}")
    if total > 0:
        print(f"Agreement rate: {100*matches/total:.1f}%")
    
    if errors and not verbose:
        print(f"\nErrors ({len(errors)}):")
        for e in errors[:5]:
            err_short = e['error'][:60].split('\n')[0]
            print(f"  {e['file']}:{e['name']} - {err_short}")
        if len(errors) > 5:
            print(f"  ... and {len(errors)-5} more")

if __name__ == "__main__":
    main()
