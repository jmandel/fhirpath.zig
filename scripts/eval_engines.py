#!/usr/bin/env python3
"""Evaluate a FHIRPath expression against multiple engines.

Usage:
  eval_engines.py <expression> [json_input]
  eval_engines.py -e js,net <expression> [json_input]

Options:
  -e, --engines   Comma-separated list of engines: js, net, hapi, all (default: all)
  -q, --quiet     Just show results, no headers

Examples:
  eval_engines.py '1 + 2'
  eval_engines.py 'name.given' '{"name":{"given":["Ann","Bob"]}}'
  eval_engines.py -e js,net '1.5.round()'
  eval_engines.py -e hapi '@2024-01-15 + 1 month'
"""

import json
import subprocess
import sys
import os
from pathlib import Path

ROOT = Path(__file__).parent.parent

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
    },
    "hapi": {
        "name": "HAPI FHIR",
        "cmd": ["java", "-jar", str(ROOT / "vendor" / "fhirpath.hapi" / "target" / "fhirpath-runner-1.0-SNAPSHOT.jar"), "--batch"],
        "env_extra": {}
    }
}

def run_engine(engine_key: str, expr: str, input_json: dict) -> dict:
    """Run expression through an engine."""
    engine = ENGINES[engine_key]
    batch_input = [{"expr": expr, "input": input_json}]
    
    try:
        result = subprocess.run(
            engine["cmd"],
            input=json.dumps(batch_input),
            capture_output=True,
            text=True,
            timeout=60,
            env={**os.environ, **engine["env_extra"]}
        )
        if result.returncode != 0:
            return {"error": result.stderr.strip()[:200] or "Unknown error"}
        results = json.loads(result.stdout)
        return results[0] if results else {"error": "No result"}
    except subprocess.TimeoutExpired:
        return {"error": "timeout"}
    except Exception as e:
        return {"error": str(e)[:200]}

def format_result(result: dict) -> str:
    """Format a result for display."""
    if "error" in result:
        return f"ERROR: {result['error'][:80]}"
    return json.dumps(result.get("results", []))

def main():
    import argparse
    parser = argparse.ArgumentParser(description="Evaluate FHIRPath expression against engines")
    parser.add_argument("expression", help="FHIRPath expression to evaluate")
    parser.add_argument("input", nargs="?", default="{}", help="JSON input (default: {})")
    parser.add_argument("-e", "--engines", default="all", help="Engines: js,net,hapi or all")
    parser.add_argument("-q", "--quiet", action="store_true", help="Minimal output")
    args = parser.parse_args()
    
    # Parse engines
    if args.engines == "all":
        engine_keys = list(ENGINES.keys())
    else:
        engine_keys = [e.strip() for e in args.engines.split(",")]
        for k in engine_keys:
            if k not in ENGINES:
                print(f"Unknown engine: {k}. Available: {', '.join(ENGINES.keys())}")
                return 1
    
    # Parse input JSON
    try:
        input_json = json.loads(args.input)
    except json.JSONDecodeError as e:
        print(f"Invalid JSON input: {e}")
        return 1
    
    if not args.quiet:
        print(f"Expression: {args.expression}")
        if input_json:
            input_str = json.dumps(input_json)
            if len(input_str) > 60:
                input_str = input_str[:57] + "..."
            print(f"Input: {input_str}")
        print()
    
    # Run each engine
    for key in engine_keys:
        result = run_engine(key, args.expression, input_json)
        if args.quiet:
            print(f"{key}: {format_result(result)}")
        else:
            print(f"{ENGINES[key]['name']:<12} {format_result(result)}")
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
