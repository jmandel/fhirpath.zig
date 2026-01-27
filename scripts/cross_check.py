#!/usr/bin/env python3
"""Cross-check artisinal tests against fhirpath.js reference implementation.

Usage:
  cross_check.py [options] [file_pattern]

Options:
  -v, --verbose     Show all results, not just differences
  -n N              Limit to N tests
  --only-passing    Only check tests that pass in our harness
  
Examples:
  cross_check.py                    # Check all artisinal tests
  cross_check.py basic              # Check tests matching 'basic'
  cross_check.py -v substring       # Verbose output for substring tests
"""

import json
import subprocess
import sys
import os
from pathlib import Path

ROOT = Path(__file__).parent.parent
FHIRPATH_JS = ROOT / "vendor" / "fhirpath.js" / "cli.ts"
ARTISINAL_DIR = ROOT / "tests" / "artisinal"

def run_fhirpath_js(expr: str, input_json: dict) -> tuple[list, str | None]:
    """Run expression through fhirpath.js, return (results, error)."""
    try:
        result = subprocess.run(
            ["bun", str(FHIRPATH_JS), "-t", expr, json.dumps(input_json)],
            capture_output=True,
            text=True,
            timeout=10,
            env={**os.environ, "PATH": f"{os.environ['HOME']}/.bun/bin:{os.environ.get('PATH', '')}"}
        )
        if result.returncode != 0:
            return [], result.stderr.strip()
        return json.loads(result.stdout), None
    except subprocess.TimeoutExpired:
        return [], "timeout"
    except Exception as e:
        return [], str(e)

def normalize_value(v):
    """Normalize values for comparison."""
    if isinstance(v, dict):
        if "type" in v and "value" in v:
            # Typed format - normalize the value
            val = v["value"]
            typ = v["type"]
            # Normalize type names
            if typ in ("System.Boolean", "boolean"):
                typ = "boolean"
            elif typ in ("System.Integer", "integer"):
                typ = "integer"
            elif typ in ("System.Decimal", "decimal"):
                typ = "decimal"
            elif typ in ("System.String", "string"):
                typ = "string"
            return {"type": typ, "value": val}
        return {k: normalize_value(vv) for k, vv in v.items()}
    elif isinstance(v, list):
        return [normalize_value(x) for x in v]
    return v

def compare_results(expected: list, actual: list) -> tuple[bool, str]:
    """Compare expected vs actual results. Returns (match, reason)."""
    exp_norm = normalize_value(expected)
    act_norm = normalize_value(actual)
    
    if exp_norm == act_norm:
        return True, "exact match"
    
    # Check if counts match but order differs
    if len(exp_norm) == len(act_norm):
        if sorted(json.dumps(x, sort_keys=True) for x in exp_norm) == \
           sorted(json.dumps(x, sort_keys=True) for x in act_norm):
            return True, "match (order differs)"
    
    return False, f"expected {exp_norm}, got {act_norm}"

def load_tests(file_path: Path) -> list[dict]:
    """Load test cases from a file."""
    with open(file_path) as f:
        data = json.load(f)
    cases = data.get("cases", data.get("tests", []))
    return [c for c in cases if "expr" in c or "expression" in c]

def main():
    args = sys.argv[1:]
    verbose = "-v" in args or "--verbose" in args
    args = [a for a in args if a not in ("-v", "--verbose")]
    
    limit = None
    if "-n" in args:
        idx = args.index("-n")
        limit = int(args[idx + 1])
        args = args[:idx] + args[idx+2:]
    
    pattern = args[0] if args else ""
    
    # Find test files
    test_files = sorted(ARTISINAL_DIR.glob("*.json"))
    if pattern:
        test_files = [f for f in test_files if pattern in f.name]
    
    total = 0
    matches = 0
    diffs = []
    errors = []
    
    for test_file in test_files:
        cases = load_tests(test_file)
        for case in cases:
            if limit and total >= limit:
                break
                
            name = case.get("name", "unnamed")
            expr = case.get("expr") or case.get("expression")
            input_json = case.get("input", {})
            expected = case.get("expect", [])
            
            if not expr:
                continue
            
            total += 1
            
            # Run through fhirpath.js
            actual, err = run_fhirpath_js(expr, input_json)
            
            if err:
                errors.append({
                    "file": test_file.name,
                    "name": name,
                    "expr": expr,
                    "error": err
                })
                if verbose:
                    print(f"[ERROR] {test_file.name}:{name}")
                    print(f"  expr: {expr}")
                    print(f"  error: {err}")
                continue
            
            match, reason = compare_results(expected, actual)
            
            if match:
                matches += 1
                if verbose:
                    print(f"[MATCH] {test_file.name}:{name} - {reason}")
            else:
                diffs.append({
                    "file": test_file.name,
                    "name": name,
                    "expr": expr,
                    "input": input_json,
                    "expected": expected,
                    "fhirpath_js": actual,
                    "reason": reason
                })
                print(f"[DIFF] {test_file.name}:{name}")
                print(f"  expr: {expr}")
                if input_json != {}:
                    print(f"  input: {json.dumps(input_json)}")
                print(f"  our expected: {json.dumps(expected)}")
                print(f"  fhirpath.js:  {json.dumps(actual)}")
                print()
        
        if limit and total >= limit:
            break
    
    print("=" * 60)
    print(f"Total: {total}, Matches: {matches}, Diffs: {len(diffs)}, Errors: {len(errors)}")
    print(f"Agreement rate: {100*matches/total:.1f}%" if total > 0 else "No tests")
    
    if errors and not verbose:
        print(f"\nErrors ({len(errors)}):")
        for e in errors[:5]:
            print(f"  {e['file']}:{e['name']} - {e['error'][:50]}")
        if len(errors) > 5:
            print(f"  ... and {len(errors)-5} more")

if __name__ == "__main__":
    main()
