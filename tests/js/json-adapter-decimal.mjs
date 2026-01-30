#!/usr/bin/env node
/**
 * Regression test: JSON adapter mode must handle decimal numbers correctly.
 *
 * When input is a pre-parsed JS object (adapter mode), decimal numbers must
 * remain usable for arithmetic — they should not be wrapped in objects that
 * the WASM engine can't operate on.
 *
 * Usage:  node tests/js/json-adapter-decimal.mjs
 */

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT = path.resolve(__dirname, "../..");

const { FhirPathEngine } = await import(path.join(ROOT, "js/fhirpath.js"));

const engine = await FhirPathEngine.instantiate({
  wasmBytes: fs.readFileSync(path.join(ROOT, "zig-out/bin/fhirpath.wasm")),
  schemas: [
    {
      name: "r5",
      prefix: "FHIR",
      model: fs.readFileSync(path.join(ROOT, "models/r5/model.bin")),
      isDefault: true,
    },
  ],
});

const input = {
  resourceType: "Observation",
  status: "final",
  valueQuantity: { value: 98.6, unit: "degF" },
};

let failures = 0;

function assert(label, actual, expected) {
  if (actual === expected) {
    console.log(`  PASS: ${label}`);
  } else {
    console.error(`  FAIL: ${label}`);
    console.error(`    expected: ${JSON.stringify(expected)}`);
    console.error(`    actual:   ${JSON.stringify(actual)}`);
    failures++;
  }
}

console.log("JSON adapter decimal tests:");

// Test 1: reading a decimal value via adapter produces a result
{
  const results = [...engine.eval({ expr: "value.ofType(Quantity).value", input, schema: "r5" })];
  assert("value.ofType(Quantity).value returns 1 result", results.length, 1);
  if (results.length > 0) {
    const val = results[0].data;
    assert("value is string '98.6' (decimal)", val, "98.6");
  }
}

// Test 2: arithmetic on adapter-parsed decimals works
{
  const results = [...engine.eval({ expr: "value.ofType(Quantity).value * 2", input, schema: "r5" })];
  assert("value * 2 returns 1 result", results.length, 1);
  if (results.length > 0) {
    const val = results[0].data;
    assert("98.6 * 2 = '197.2'", val, "197.2");
  }
}

// Test 3: expression-literal precision preserved (e.g. * 2.000)
{
  const results = [...engine.eval({ expr: "value.ofType(Quantity).value * 2.000", input, schema: "r5" })];
  assert("value * 2.000 returns 1 result", results.length, 1);
  if (results.length > 0) {
    const val = results[0].data;
    assert("98.6 * 2.000 = '197.2000'", val, "197.2000");
  }
}

// Test 4: all three modes agree on arithmetic results
{
  const jsonText = JSON.stringify(input);
  const expr = "value.ofType(Quantity).value * 2.000";
  const wasmResults = [...engine.eval({ expr, input: jsonText, adapter: "wasm", schema: "r5" })];
  const jsResults = [...engine.eval({ expr, input, schema: "r5" })];
  assert("WASM and JS adapter agree",
    wasmResults[0]?.data, jsResults[0]?.data);
}

// Test 5: decimal wrapper preserves trailing zeros through adapter
{
  // Simulate what decimalAwareJsonParse does: wrap numbers with '.' as objects
  // that preserve the original text via toString(), with numeric valueOf()
  class JsonDecimal {
    constructor(text) { this.text = text; }
    toString() { return this.text; }
    valueOf() { return parseFloat(this.text); }
  }
  const precisionInput = {
    resourceType: "Observation",
    status: "final",
    valueQuantity: { value: new JsonDecimal("98.6000"), unit: "degF" },
  };
  const results = [...engine.eval({ expr: "value.ofType(Quantity).value", input: precisionInput, schema: "r5" })];
  assert("decimal wrapper returns 1 result", results.length, 1);
  if (results.length > 0) {
    assert("98.6000 precision preserved", results[0].data, "98.6000");
  }
}

// Test 6: decimal wrapper arithmetic preserves precision
{
  class JsonDecimal {
    constructor(text) { this.text = text; }
    toString() { return this.text; }
    valueOf() { return parseFloat(this.text); }
  }
  const precisionInput = {
    resourceType: "Observation",
    status: "final",
    valueQuantity: { value: new JsonDecimal("98.6000"), unit: "degF" },
  };
  const results = [...engine.eval({ expr: "value.ofType(Quantity).value * 2.0", input: precisionInput, schema: "r5" })];
  assert("decimal wrapper arithmetic returns 1 result", results.length, 1);
  if (results.length > 0) {
    assert("98.6000 * 2.0 = '197.20000'", results[0].data, "197.20000");
  }
}

// Test 7: integer values still work in adapter mode
{
  const intInput = {
    resourceType: "Observation",
    status: "final",
    valueQuantity: { value: 10, unit: "kg" },
  };
  const results = [...engine.eval({ expr: "value.ofType(Quantity).value * 3", input: intInput, schema: "r5" })];
  assert("integer value * 3 returns 1 result", results.length, 1);
  if (results.length > 0) {
    const val = results[0].data;
    assert("10 * 3 = 30", val, 30);
  }
}

console.log();
if (failures > 0) {
  console.error(`${failures} test(s) FAILED`);
  process.exit(1);
} else {
  console.log("All tests passed");
}
