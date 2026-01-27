#!/usr/bin/env bun
// CLI wrapper for fhirpath.js to compare against our implementation
import fhirpath from "fhirpath";

function printUsage() {
  console.log(`Usage: cli.ts [options] <expression> [json]

Evaluate a FHIRPath expression using fhirpath.js

Options:
  -t, --typed    Output {type, value} format (like our harness)
  -h, --help     Show this help

Examples:
  cli.ts 'true'
  cli.ts 'name.given' '{"name":{"given":["Ann","Bob"]}}'
  cli.ts -t '1 + 2'
  echo '{"x": 1}' | cli.ts 'x'
`);
}

function getType(value: any): string {
  if (value === null || value === undefined) return "null";
  if (typeof value === "boolean") return "boolean";
  if (typeof value === "number") {
    return Number.isInteger(value) ? "integer" : "decimal";
  }
  if (typeof value === "string") return "string";
  if (Array.isArray(value)) return "array";
  if (typeof value === "object") return "object";
  return "unknown";
}

function formatTyped(results: any[]): any[] {
  return results.map(v => ({
    type: getType(v),
    value: v
  }));
}

async function main() {
  const args = process.argv.slice(2);
  let typed = false;
  let expression = "";
  let jsonInput = "{}";

  // Parse args
  const positional: string[] = [];
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === "-h" || arg === "--help") {
      printUsage();
      process.exit(0);
    } else if (arg === "-t" || arg === "--typed") {
      typed = true;
    } else {
      positional.push(arg);
    }
  }

  if (positional.length === 0) {
    // Try reading from stdin
    const stdin = await Bun.stdin.text();
    if (stdin.trim()) {
      // Check if it's JSON (input) or expression
      try {
        JSON.parse(stdin);
        jsonInput = stdin;
      } catch {
        expression = stdin.trim();
      }
    }
    if (!expression) {
      printUsage();
      process.exit(1);
    }
  } else {
    expression = positional[0];
    if (positional.length > 1) {
      jsonInput = positional[1];
    }
  }

  let input: any;
  try {
    input = JSON.parse(jsonInput);
  } catch (e) {
    console.error(`Invalid JSON input: ${e}`);
    process.exit(1);
  }

  try {
    const results = fhirpath.evaluate(input, expression);
    if (typed) {
      console.log(JSON.stringify(formatTyped(results), null, 2));
    } else {
      console.log(JSON.stringify(results, null, 2));
    }
  } catch (e: any) {
    console.error(`FHIRPath error: ${e.message || e}`);
    process.exit(1);
  }
}

main();
