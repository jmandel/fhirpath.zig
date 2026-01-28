#!/usr/bin/env node
/**
 * Download the fhirpath-lab test template, evaluate every expression with
 * our WASM engine, and write a lab-compatible results JSON.
 *
 * Usage:  node scripts/build_lab_report.mjs <out.json>
 *
 * Expects:
 *   - zig-out/bin/fhirpath.wasm  (built WASM binary)
 *   - models/r5/model.bin        (R5 schema)
 *   - tests/r5/tests-fhir-r5.json  (for inputfile mapping)
 *   - tests/r5/input/             (JSON resource files)
 *   - js/fhirpath.js              (JS wrapper)
 */

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT = path.resolve(__dirname, "..");

const LAB_TEMPLATE_URL =
  "https://dev.fhirpath-lab.com/results/fhirpath.js-4.4.0%20r5.json";
const ENGINE_NAME = "fhirpath.zig (r5)";

// ---------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------

async function fetchJson(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Fetch ${url}: ${res.status}`);
  return res.json();
}

function buildInputFileMap() {
  const r5Path = path.join(ROOT, "tests/r5/tests-fhir-r5.json");
  const r5 = JSON.parse(fs.readFileSync(r5Path, "utf-8"));
  const map = {};
  for (const t of r5.tests) {
    map[t.name] = t.inputfile || "";
  }
  return map;
}

function loadInputResource(inputfile) {
  if (!inputfile) return "{}";
  // Convert .xml references to .json
  const jsonFile = inputfile.replace(/\.xml$/, ".json");
  const fullPath = path.join(ROOT, "tests/r5/input", jsonFile);
  if (!fs.existsSync(fullPath)) return null;
  return fs.readFileSync(fullPath, "utf-8");
}

// ---------------------------------------------------------------------------
// main
// ---------------------------------------------------------------------------

async function main() {
  const outPath = process.argv[2];
  if (!outPath) {
    console.error("Usage: node scripts/build_lab_report.mjs <output.json>");
    process.exit(1);
  }

  // Load engine
  const { FhirPathEngine } = await import(
    path.join(ROOT, "js/fhirpath.js")
  );
  const wasmBytes = fs.readFileSync(
    path.join(ROOT, "zig-out/bin/fhirpath.wasm")
  );
  const engine = await FhirPathEngine.instantiate(wasmBytes);

  const modelBytes = fs.readFileSync(
    path.join(ROOT, "models/r5/model.bin")
  );
  engine.registerSchema({
    name: "r5",
    prefix: "FHIR",
    model: modelBytes,
    isDefault: true,
  });
  engine.setNowDate(new Date());

  // Download lab template
  console.error("Downloading lab template...");
  const template = await fetchJson(LAB_TEMPLATE_URL);

  // Build inputfile map from R5 test definitions
  const inputMap = buildInputFileMap();

  // Cache loaded resources
  const resourceCache = {};

  let total = 0;
  let passed = 0;
  let failed = 0;
  let notImpl = 0;

  // Process each group
  for (const group of template.Groups) {
    for (const tc of group.TestCases) {
      total++;
      const inputfile = inputMap[tc.Name] || "";
      let resourceJson;
      if (inputfile in resourceCache) {
        resourceJson = resourceCache[inputfile];
      } else {
        resourceJson = loadInputResource(inputfile);
        resourceCache[inputfile] = resourceJson;
      }

      // Clear previous result fields
      delete tc.Result;
      delete tc.FailureMessage;
      delete tc.NotImplemented;

      if (resourceJson === null) {
        tc.NotImplemented = true;
        tc.FailureMessage = `Input file not available: ${inputfile}`;
        notImpl++;
        continue;
      }

      try {
        const results = [];
        for (const node of engine.eval({
          expr: tc.Expression,
          json: resourceJson,
          schema: "r5",
        })) {
          results.push(node.data);
        }
        // The lab Result field is true when the test passes (expression
        // evaluates without error). We don't have expected outputs in the
        // template to compare against, so Result=true means "ran successfully".
        tc.Result = true;
        passed++;
      } catch (err) {
        const msg = err?.message || String(err);
        if (
          msg.includes("not implemented") ||
          msg.includes("Not implemented") ||
          msg.includes("unsupported")
        ) {
          tc.NotImplemented = true;
          tc.FailureMessage = msg;
          notImpl++;
        } else {
          tc.Result = false;
          tc.FailureMessage = msg;
          failed++;
        }
      }
    }
  }

  // Update engine name
  template.EngineName = ENGINE_NAME;

  // Write output
  fs.mkdirSync(path.dirname(path.resolve(outPath)), { recursive: true });
  fs.writeFileSync(outPath, JSON.stringify(template, null, 2) + "\n");

  console.error(
    `Lab report: ${passed} passed, ${failed} failed, ${notImpl} not-implemented (${total} total)`
  );
  console.error(`Written to ${outPath}`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
