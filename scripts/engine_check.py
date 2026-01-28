#!/usr/bin/env python3
"""Cross-check artisinal tests against multiple FHIRPath reference implementations.

Supported engines:
  - js: fhirpath.js (JavaScript/Bun)
  - net: Firely .NET SDK (C#/.NET)

Usage:
  engine_check.py [options] [file_pattern]

Options:
  -e, --engine ENGINE    Engine to use: js, net, all (default: all)
  -v, --verbose          Show all results, not just differences
  -n N                   Limit to N tests  
  --sample N             Random sample of N differences
  --summary              Show summary only (no individual test results)
  
Examples:
  engine_check.py                      # Check all tests with all engines
  engine_check.py -e net               # Check with .NET engine only
  engine_check.py -e js basic          # Check 'basic' tests with JS engine
  engine_check.py --summary            # Just show pass/fail counts per engine
"""

import json
import subprocess
import sys
import os
import random
from pathlib import Path
from typing import Optional
from dataclasses import dataclass, field

ROOT = Path(__file__).parent.parent
ARTISINAL_DIR = ROOT / "tests" / "artisinal"

# Engine configurations
ENGINES = {
    "js": {
        "name": "fhirpath.js",
        "cmd": ["bun", str(ROOT / "vendor" / "fhirpath.js" / "cli.ts"), "--batch"],
        "env_extra": {"PATH": f"{os.environ['HOME']}/.bun/bin:{os.environ.get('PATH', '')}"}
    },
    "net": {
        "name": "Firely .NET",
        "cmd": ["dotnet", "run", "--project", str(ROOT / "vendor" / "fhirpath.net" / "FhirPathRunner"), "--", "--batch"],
        "env_extra": {}
    }
}

@dataclass
class TestResult:
    name: str
    expr: str
    our_expected: list
    engine_results: dict = field(default_factory=dict)  # engine -> {results: [...], error: str}
    
def run_engine_batch(engine_key: str, tests: list[dict], chunk_size: int = 100) -> list[dict]:
    """Run multiple tests through an engine in batched calls."""
    if not tests:
        return []
    
    engine = ENGINES[engine_key]
    all_results = []
    
    # Process in chunks to avoid timeout/memory issues
    for i in range(0, len(tests), chunk_size):
        chunk = tests[i:i + chunk_size]
        batch_input = [{"expr": t["expr"], "input": t.get("input", {})} for t in chunk]
        
        try:
            result = subprocess.run(
                engine["cmd"],
                input=json.dumps(batch_input),
                capture_output=True,
                text=True,
                timeout=120,
                env={**os.environ, **engine["env_extra"]}
            )
            if result.returncode != 0:
                all_results.extend([{"error": result.stderr.strip() or "Unknown error"}] * len(chunk))
            else:
                all_results.extend(json.loads(result.stdout))
        except subprocess.TimeoutExpired:
            all_results.extend([{"error": "timeout"}] * len(chunk))
        except Exception as e:
            all_results.extend([{"error": str(e)}] * len(chunk))
    
    return all_results

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
        "number": "decimal",  # JS uses 'number'
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

def results_match(expected: list, actual: list) -> tuple[bool, str]:
    """Compare expected vs actual results. Returns (match, reason)."""
    if len(expected) != len(actual):
        return False, f"length mismatch: expected {len(expected)}, got {len(actual)}"
    
    for i, (exp, act) in enumerate(zip(expected, actual)):
        exp_type = normalize_type(exp.get("type", ""))
        act_type = normalize_type(act.get("type", ""))
        
        # Allow number vs integer/decimal
        type_ok = (exp_type == act_type or 
                   (exp_type in ("integer", "decimal") and act_type in ("integer", "decimal", "number")))
        
        if not type_ok:
            return False, f"item {i}: type mismatch (expected {exp_type}, got {act_type})"
        
        exp_val = normalize_value(exp.get("value"))
        act_val = normalize_value(act.get("value"))
        
        if exp_val != act_val:
            return False, f"item {i}: value mismatch (expected {exp_val}, got {act_val})"
    
    return True, "match"

def load_artisinal_tests(pattern: Optional[str] = None) -> list[tuple[str, dict]]:
    """Load artisinal test cases, optionally filtered by pattern."""
    tests = []
    
    for test_file in sorted(ARTISINAL_DIR.glob("*.json")):
        if pattern and pattern.lower() not in test_file.name.lower():
            continue
        
        try:
            data = json.loads(test_file.read_text())
        except json.JSONDecodeError:
            continue
        
        cases = data.get("cases", [])
        for case in cases:
            # Skip non-test entries (comments, etc)
            if "expr" not in case:
                continue
            if case.get("expect_error"):
                continue  # Skip error tests for now
            tests.append((test_file.name, case))
    
    return tests

def main():
    import argparse
    parser = argparse.ArgumentParser(description="Cross-check tests against FHIRPath engines")
    parser.add_argument("pattern", nargs="?", help="Filter test files by pattern")
    parser.add_argument("-e", "--engine", default="all", help="Engine: js, net, all")
    parser.add_argument("-v", "--verbose", action="store_true", help="Show all results")
    parser.add_argument("-n", type=int, help="Limit number of tests")
    parser.add_argument("--sample", type=int, help="Random sample of N differences")
    parser.add_argument("--summary", action="store_true", help="Summary only")
    args = parser.parse_args()
    
    # Determine which engines to use
    if args.engine == "all":
        engine_keys = list(ENGINES.keys())
    elif args.engine in ENGINES:
        engine_keys = [args.engine]
    else:
        print(f"Unknown engine: {args.engine}. Available: {', '.join(ENGINES.keys())}, all")
        return 1
    
    # Load tests
    all_tests = load_artisinal_tests(args.pattern)
    if args.n:
        all_tests = all_tests[:args.n]
    
    if not all_tests:
        print("No tests found.")
        return 1
    
    print(f"Running {len(all_tests)} tests against: {', '.join(ENGINES[k]['name'] for k in engine_keys)}")
    print()
    
    # Run tests through each engine
    engine_results = {}
    for engine_key in engine_keys:
        print(f"Running {ENGINES[engine_key]['name']}...", end=" ", flush=True)
        test_dicts = [t[1] for t in all_tests]
        engine_results[engine_key] = run_engine_batch(engine_key, test_dicts)
        print("done")
    
    # Analyze results
    stats = {k: {"match": 0, "mismatch": 0, "error": 0} for k in engine_keys}
    differences = []
    
    for i, (filename, test) in enumerate(all_tests):
        test_name = test.get("name", f"test_{i}")
        expected = test.get("expect", [])
        expr = test.get("expr", "")
        
        for engine_key in engine_keys:
            result = engine_results[engine_key][i]
            
            if "error" in result:
                stats[engine_key]["error"] += 1
                if not args.summary:
                    differences.append({
                        "file": filename,
                        "name": test_name,
                        "expr": expr,
                        "engine": engine_key,
                        "expected": expected,
                        "actual": None,
                        "error": result["error"],
                        "reason": "error"
                    })
            else:
                actual = result.get("results", [])
                match, reason = results_match(expected, actual)
                
                if match:
                    stats[engine_key]["match"] += 1
                else:
                    stats[engine_key]["mismatch"] += 1
                    if not args.summary:
                        differences.append({
                            "file": filename,
                            "name": test_name,
                            "expr": expr,
                            "engine": engine_key,
                            "expected": expected,
                            "actual": actual,
                            "error": None,
                            "reason": reason
                        })
    
    # Print summary
    print()
    print("=" * 70)
    print(f"{'Engine':<20} {'Match':>10} {'Mismatch':>10} {'Error':>10} {'Rate':>10}")
    print("-" * 70)
    for engine_key in engine_keys:
        s = stats[engine_key]
        total = s["match"] + s["mismatch"] + s["error"]
        rate = f"{100*s['match']/total:.1f}%" if total > 0 else "N/A"
        print(f"{ENGINES[engine_key]['name']:<20} {s['match']:>10} {s['mismatch']:>10} {s['error']:>10} {rate:>10}")
    print("=" * 70)
    
    # Print differences
    if not args.summary and differences:
        if args.sample:
            differences = random.sample(differences, min(args.sample, len(differences)))
        
        print(f"\nDifferences ({len(differences)} shown):")
        print()
        
        for diff in differences:
            print(f"[{diff['engine']}] {diff['file']}:{diff['name']}")
            print(f"  expr: {diff['expr'][:70]}{'...' if len(diff['expr']) > 70 else ''}")
            if diff['error']:
                print(f"  error: {diff['error'][:100]}")
            else:
                print(f"  expected: {json.dumps(diff['expected'])[:80]}")
                print(f"  actual:   {json.dumps(diff['actual'])[:80]}")
                print(f"  reason: {diff['reason']}")
            print()
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
