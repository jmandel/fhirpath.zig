#!/usr/bin/env bun
// CLI wrapper for fhirpath.js to compare against our implementation
import fhirpath from "fhirpath";
import * as types from "fhirpath/src/types";

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
`);
}

function getTypeAndValue(r: any): { type: string; value: any } {
  // Check FHIRPath internal types first (when resolveInternalTypes: false)
  if (r instanceof types.FP_Quantity) {
    return {
      type: "Quantity",
      value: { value: r.value, unit: r.unit.replace(/^'|'$/g, '') }
    };
  }
  if (r instanceof types.FP_DateTime) {
    return { type: "dateTime", value: r.toString() };
  }
  if (r instanceof types.FP_Date) {
    return { type: "date", value: r.toString() };
  }
  if (r instanceof types.FP_Time) {
    return { type: "time", value: r.toString() };
  }
  
  // Primitive types
  if (r === null || r === undefined) return { type: "null", value: null };
  if (typeof r === "boolean") return { type: "boolean", value: r };
  if (typeof r === "number") {
    // NOTE: JavaScript cannot distinguish Integer from Decimal - both are Number
    // We report "number" to be honest about this limitation
    return { type: "number", value: r };
  }
  if (typeof r === "string") return { type: "string", value: r };
  if (Array.isArray(r)) return { type: "array", value: r };
  if (typeof r === "object") return { type: "object", value: r };
  return { type: "unknown", value: r };
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
    printUsage();
    process.exit(1);
  }

  expression = positional[0];
  if (positional.length > 1) {
    jsonInput = positional[1];
  }

  let input: any;
  try {
    input = JSON.parse(jsonInput);
  } catch (e) {
    console.error(`Invalid JSON input: ${e}`);
    process.exit(1);
  }

  try {
    // Use resolveInternalTypes: false to get proper FP_* types for Quantity, DateTime, etc.
    const results = fhirpath.evaluate(input, expression, null, null, { resolveInternalTypes: false });
    
    if (typed) {
      const typedResults = results.map(r => getTypeAndValue(r));
      console.log(JSON.stringify(typedResults, null, 2));
    } else {
      // For non-typed output, resolve to simple values
      const simpleResults = fhirpath.evaluate(input, expression);
      console.log(JSON.stringify(simpleResults, null, 2));
    }
  } catch (e: any) {
    console.error(`FHIRPath error: ${e.message || e}`);
    process.exit(1);
  }
}

main();
