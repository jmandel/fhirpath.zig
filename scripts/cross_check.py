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
  cross_check.py -v substring       # Verbose output for substring tests
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

def run_fhirpath_js(expr: str, input_json: dict) -> tuple[list, str | None]:
    """Run expression through fhirpath.js with typed output, return (results, error)."""
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
        "unknown": "unknown",
    }
    return mappings.get(t, t)

def normalize_value(v):
    """Normalize a value for comparison."""
    if isinstance(v, float):
        # Round to avoid FP precision issues
        rounded = round(v, 10)
        # Treat whole floats as ints for comparison
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
    
    # JS reports "number" for both integer and decimal
    if js_type == "number" and our_type in ("integer", "decimal"):
        return True
    
    return False

def compare_results(expected: list, actual: list) -> tuple[bool, str]:
    """Compare expected vs actual results. Returns (match, reason)."""
    if len(expected) != len(actual):
        return False, f"length mismatch: expected {len(expected)}, got {len(actual)}"
    
    for i, (e, a) in enumerate(zip(expected, actual)):
        # Extract types
        e_type = normalize_type(e.get("type", "unknown") if isinstance(e, dict) else "unknown")
        a_type = normalize_type(a.get("type", "unknown") if isinstance(a, dict) else "unknown")
        
        # Check type compatibility
        if not types_compatible(e_type, a_type):
            return False, f"type mismatch at [{i}]: expected {e_type}, got {a_type}"
        
        # Compare values
        e_val = extract_value(e)
        a_val = extract_value(a)
        
        if e_val != a_val:
            return False, f"value mismatch at [{i}]: expected {e_val}, got {a_val}"
    
    return True, "match"

def load_tests(file_path: Path, include_adjudicated: bool = False) -> list[dict]:
    """Load test cases from a file."""
    with open(file_path) as f:
        data = json.load(f)
    cases = data.get("cases", data.get("tests", []))
    result = []
    for c in cases:
        if not ("expr" in c or "expression" in c):
            continue
        if not include_adjudicated and "_adjudicated" in c:
            continue
        result.append(c)
    return result

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
    
    total = 0
    matches = 0
    diffs = []
    errors = []
    
    for test_file in test_files:
        cases = load_tests(test_file, include_adjudicated)
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
                    "input": input_json,
                    "expected": expected,
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
                    print(f"[MATCH] {test_file.name}:{name}")
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
        
        if limit and total >= limit:
            break
    
    # If sampling, pick random diffs
    if sample_size and diffs:
        sampled = random.sample(diffs, min(sample_size, len(diffs)))
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
    elif sample_size and errors:
        # If no diffs but there are errors, sample those
        sampled = random.sample(errors, min(sample_size, len(errors)))
        print(f"=== SAMPLED {len(sampled)} ERRORS FOR REVIEW ===\n")
        for e in sampled:
            print(f"[ERROR] {e['file']}:{e['name']}")
            print(f"  expr: {e['expr']}")
            if e['input'] != {}:
                print(f"  input: {json.dumps(e['input'])}")
            print(f"  our expected: {json.dumps(e['expected'])}")
            print(f"  fhirpath.js error: {e['error']}")
            print()
    else:
        # Normal output - show all diffs
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
    
    print("=" * 60)
    print(f"Total: {total}, Matches: {matches}, Diffs: {len(diffs)}, Errors: {len(errors)}")
    if total > 0:
        print(f"Agreement rate: {100*matches/total:.1f}%")
    
    if errors and not verbose and not sample_size:
        print(f"\nErrors ({len(errors)}) - features not implemented in fhirpath.js:")
        for e in errors[:5]:
            err_short = e['error'][:60].split('\n')[0]
            print(f"  {e['file']}:{e['name']} - {err_short}")
        if len(errors) > 5:
            print(f"  ... and {len(errors)-5} more")

if __name__ == "__main__":
    main()
