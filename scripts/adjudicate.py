#!/usr/bin/env python3
"""Sample disagreements between our expected values and reference engines.

Strategy:
1. Run fast engines (js, net) to find disagreements with our expected values
2. Sample N disagreements (where at least one engine returns a different result)
3. For sampled items only, also run slow engines (hapi) to get full picture
4. Output detailed report for adjudication

Usage:
  adjudicate.py [options] [file_pattern]

Options:
  --sample N       Number of disagreements to sample (default: 5)
  --skip-slow      Don't run slow engines (hapi) on samples
  -v, --verbose    Show more detail
  
Examples:
  adjudicate.py                    # Sample 5 disagreements, all engines
  adjudicate.py --sample 3         # Sample 3 disagreements
  adjudicate.py --skip-slow        # Only use fast engines
  adjudicate.py comparison         # Filter to comparison tests
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

# Engine configurations - split by speed
FAST_ENGINES = {
    "js": {
        "name": "fhirpath.js",
        "cmd": ["bun", str(ROOT / "vendor" / "fhirpath.js" / "cli.ts"), "--batch"],
        "env_extra": {"PATH": f"{os.environ['HOME']}/.bun/bin:{os.environ.get('PATH', '')}"}
    },
    "net": {
        "name": "Firely .NET",
        "cmd": ["dotnet", "run", "--project", str(ROOT / "vendor" / "fhirpath.net" / "FhirPathRunner"), "--", "--batch"],
        "env_extra": {}
    },
}

SLOW_ENGINES = {
    "hapi": {
        "name": "HAPI FHIR",
        "cmd": ["java", "-jar", str(ROOT / "vendor" / "fhirpath.hapi" / "target" / "fhirpath-runner-1.0-SNAPSHOT.jar"), "--batch"],
        "env_extra": {}
    }
}

def run_engine_batch(engine_config: dict, tests: list[dict], chunk_size: int = 100) -> list[dict]:
    """Run multiple tests through an engine in batched calls."""
    if not tests:
        return []
    
    all_results = []
    
    for i in range(0, len(tests), chunk_size):
        chunk = tests[i:i + chunk_size]
        batch_input = [{"expr": t["expr"], "input": t.get("input", {})} for t in chunk]
        
        try:
            result = subprocess.run(
                engine_config["cmd"],
                input=json.dumps(batch_input),
                capture_output=True,
                text=True,
                timeout=120,
                env={**os.environ, **engine_config["env_extra"]}
            )
            if result.returncode != 0:
                all_results.extend([{"error": result.stderr.strip()[:200] or "Unknown error"}] * len(chunk))
            else:
                all_results.extend(json.loads(result.stdout))
        except subprocess.TimeoutExpired:
            all_results.extend([{"error": "timeout"}] * len(chunk))
        except Exception as e:
            all_results.extend([{"error": str(e)[:200]}] * len(chunk))
    
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
        "number": "decimal",
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

def results_match(expected: list, actual: list) -> bool:
    """Check if results match (ignoring type name differences)."""
    if len(expected) != len(actual):
        return False
    
    for exp, act in zip(expected, actual):
        exp_type = normalize_type(exp.get("type", ""))
        act_type = normalize_type(act.get("type", ""))
        
        type_ok = (exp_type == act_type or 
                   (exp_type in ("integer", "decimal") and act_type in ("integer", "decimal", "number")))
        
        if not type_ok:
            return False
        
        exp_val = normalize_value(exp.get("value"))
        act_val = normalize_value(act.get("value"))
        
        if exp_val != act_val:
            return False
    
    return True

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
            if "expr" not in case:
                continue
            if case.get("expect_error"):
                continue
            # Skip already adjudicated
            if "_adjudicated" in case:
                continue
            tests.append((test_file.name, case))
    
    return tests

def format_result(result: dict) -> str:
    """Format a result for display."""
    if "error" in result:
        err = result["error"]
        if len(err) > 60:
            err = err[:57] + "..."
        return f"ERROR: {err}"
    return json.dumps(result.get("results", []))

def main():
    import argparse
    parser = argparse.ArgumentParser(description="Sample disagreements for adjudication")
    parser.add_argument("pattern", nargs="?", help="Filter test files by pattern")
    parser.add_argument("--sample", type=int, default=5, help="Number of disagreements to sample")
    parser.add_argument("--skip-slow", action="store_true", help="Skip slow engines (hapi)")
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose output")
    args = parser.parse_args()
    
    # Load tests
    all_tests = load_artisinal_tests(args.pattern)
    if not all_tests:
        print("No unadjudicated tests found.")
        return 0
    
    print(f"Checking {len(all_tests)} unadjudicated tests with fast engines...")
    
    # Run fast engines
    test_dicts = [t[1] for t in all_tests]
    fast_results = {}
    for key, config in FAST_ENGINES.items():
        print(f"  Running {config['name']}...", end=" ", flush=True)
        fast_results[key] = run_engine_batch(config, test_dicts)
        print("done")
    
    # Find disagreements (where at least one fast engine returns different non-error result)
    disagreements = []
    for i, (filename, test) in enumerate(all_tests):
        expected = test.get("expect", [])
        dominated_by_errors = True
        has_disagreement = False
        
        for key in FAST_ENGINES:
            result = fast_results[key][i]
            if "error" not in result:
                dominated_by_errors = False
                actual = result.get("results", [])
                if not results_match(expected, actual):
                    has_disagreement = True
        
        if has_disagreement and not dominated_by_errors:
            disagreements.append({
                "index": i,
                "filename": filename,
                "test": test,
                "fast_results": {k: fast_results[k][i] for k in FAST_ENGINES}
            })
    
    print(f"\nFound {len(disagreements)} disagreements (non-error differences)")
    
    if not disagreements:
        print("No disagreements to adjudicate!")
        return 0
    
    # Sample disagreements
    sampled = random.sample(disagreements, min(args.sample, len(disagreements)))
    print(f"Sampled {len(sampled)} for detailed review\n")
    
    # Run slow engines on sampled items only
    if not args.skip_slow and SLOW_ENGINES:
        sampled_tests = [s["test"] for s in sampled]
        for key, config in SLOW_ENGINES.items():
            print(f"Running {config['name']} on {len(sampled)} samples...", end=" ", flush=True)
            slow_results = run_engine_batch(config, sampled_tests)
            for j, s in enumerate(sampled):
                s.setdefault("slow_results", {})[key] = slow_results[j]
            print("done")
    
    # Print detailed report
    print("\n" + "=" * 80)
    print("DISAGREEMENTS FOR ADJUDICATION")
    print("=" * 80)
    
    for j, item in enumerate(sampled, 1):
        test = item["test"]
        print(f"\n[{j}/{len(sampled)}] {item['filename']}:{test.get('name', '?')}")
        print(f"  expr: {test['expr']}")
        if test.get("input"):
            input_str = json.dumps(test["input"])
            if len(input_str) > 60:
                input_str = input_str[:57] + "..."
            print(f"  input: {input_str}")
        print()
        print(f"  OURS:      {json.dumps(test.get('expect', []))}")
        
        for key, config in FAST_ENGINES.items():
            result = item["fast_results"][key]
            print(f"  {config['name']:<10} {format_result(result)}")
        
        for key, config in SLOW_ENGINES.items():
            if "slow_results" in item and key in item["slow_results"]:
                result = item["slow_results"][key]
                print(f"  {config['name']:<10} {format_result(result)}")
        
        print()
    
    print("=" * 80)
    print(f"\nTo adjudicate, add '_adjudicated' to test cases in the source files.")
    print("See methodology.md CROSS_CHECK section for format.")
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
