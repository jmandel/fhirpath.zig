#!/usr/bin/env bun
// CLI wrapper for fhirpath.js to compare against our implementation
import fhirpath from "fhirpath";
import * as types from "fhirpath/src/types";

function getTypeAndValue(r: any): { type: string; value: any } {
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
  
  if (r === null || r === undefined) return { type: "null", value: null };
  if (typeof r === "boolean") return { type: "boolean", value: r };
  if (typeof r === "number") return { type: "number", value: r };
  if (typeof r === "string") return { type: "string", value: r };
  if (Array.isArray(r)) return { type: "array", value: r };
  if (typeof r === "object") return { type: "object", value: r };
  return { type: "unknown", value: r };
}

// Batch mode: read JSON array of {expr, input} from stdin
async function batchMode() {
  const input = await Bun.stdin.text();
  const tests: Array<{expr: string, input: any}> = JSON.parse(input);
  
  const results: Array<{results?: any[], error?: string}> = [];
  
  for (const test of tests) {
    try {
      const raw = fhirpath.evaluate(test.input || {}, test.expr, null, null, { resolveInternalTypes: false });
      results.push({ results: raw.map(r => getTypeAndValue(r)) });
    } catch (e: any) {
      results.push({ error: e.message || String(e) });
    }
  }
  
  console.log(JSON.stringify(results));
}

// Single expression mode
async function singleMode(args: string[]) {
  let typed = false;
  const positional: string[] = [];
  
  for (const arg of args) {
    if (arg === "-t" || arg === "--typed") {
      typed = true;
    } else if (arg !== "-h" && arg !== "--help") {
      positional.push(arg);
    }
  }
  
  if (positional.length === 0) {
    console.log(`Usage: cli.ts [options] <expression> [json]
       cli.ts --batch < tests.json

Options:
  -t, --typed    Output {type, value} format
  --batch        Batch mode: read [{expr, input}, ...] from stdin
`);
    process.exit(1);
  }
  
  const expression = positional[0];
  const jsonInput = positional[1] || "{}";
  
  let input: any;
  try {
    input = JSON.parse(jsonInput);
  } catch (e) {
    console.error(`Invalid JSON: ${e}`);
    process.exit(1);
  }
  
  try {
    const raw = fhirpath.evaluate(input, expression, null, null, { resolveInternalTypes: false });
    if (typed) {
      console.log(JSON.stringify(raw.map(r => getTypeAndValue(r)), null, 2));
    } else {
      const simple = fhirpath.evaluate(input, expression);
      console.log(JSON.stringify(simple, null, 2));
    }
  } catch (e: any) {
    console.error(`FHIRPath error: ${e.message || e}`);
    process.exit(1);
  }
}

const args = process.argv.slice(2);
if (args.includes("--batch")) {
  batchMode();
} else {
  singleMode(args);
}
