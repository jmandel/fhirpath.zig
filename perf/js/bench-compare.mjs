import fs from "node:fs";
import path from "node:path";
import { performance } from "node:perf_hooks";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";
import { FhirPathEngine } from "../../js/fhirpath.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const require = createRequire(import.meta.url);
const fhirpathJs = require("fhirpath");
const fhirpathR4Model = require("fhirpath/fhir-context/r5");

const ROOT = path.resolve(__dirname, "../..");

const scenarios = [
  // ── Small resources (~4-6 KB) ──
  {
    name: "patient-name.given",
    file: "tests/r5/input/patient-example.json",
    expr: "name.given",
  },
  {
    name: "patient-name.where(use=official)",
    file: "tests/r5/input/patient-example.json",
    expr: "name.where(use='official').given",
  },
  {
    name: "patient-telecom-count",
    file: "tests/r5/input/patient-example.json",
    expr: "telecom.count()",
  },
  {
    name: "patient-identifier-system",
    file: "tests/r5/input/patient-example.json",
    expr: "identifier.where(system='urn:oid:1.2.36.146.595.217.0.1').value",
  },
  {
    name: "docref-attachment-url-length",
    file: "tests/r5/input/documentreference-example.json",
    expr: "content.first().attachment.url.length()",
  },
  {
    name: "capstmt-experimental",
    file: "tests/r5/input/capabilitystatement-example.json",
    expr: "experimental",
  },
  // ── Large resources (130-240 KB) ──
  {
    name: "structdef-resourceType",
    file: "tests/r5/input/structuredefinition-language.json",
    expr: "resourceType",
  },
  {
    name: "structdef-snapshot-element-count",
    file: "tests/r5/input/structuredefinition-language.json",
    expr: "snapshot.element.count()",
  },
  {
    name: "binary-contentType",
    file: "tests/r5/input/binary-example.json",
    expr: "contentType",
  },
  {
    name: "bundle-entry-resourceType-count",
    file: "tests/r5/input/diagnosticreport-example.json",
    expr: "entry.resource.resourceType.count()",
  },
  {
    name: "bundle-entry-where-count",
    file: "tests/r5/input/diagnosticreport-example.json",
    expr: "entry.resource.where(resourceType='DiagnosticReport').count()",
  },
  {
    name: "bundle-complex-query",
    file: "tests/r5/input/diagnosticreport-example.json",
    expr: "entry.resource.where(resourceType='DiagnosticReport').result.reference.select($this.length()).sum()",
  },
];

function parseArgs() {
  const args = process.argv.slice(2);
  let warmup = 10;
  let iterations = 100;
  let scenario = null;
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === "--warmup" && i + 1 < args.length) warmup = Number(args[++i]);
    else if (arg === "--iterations" && i + 1 < args.length) iterations = Number(args[++i]);
    else if (arg === "--scenario" && i + 1 < args.length) scenario = args[++i];
    else if (arg === "--help" || arg === "-h") {
      console.log("Usage: node perf/js/bench-compare.mjs [--iterations N] [--warmup N] [--scenario name]");
      console.log("Scenarios:");
      for (const sc of scenarios) console.log(`  ${sc.name}`);
      process.exit(0);
    }
  }
  return { warmup, iterations, scenario };
}

function checksumValue(value) {
  if (Array.isArray(value)) return value.reduce((acc, v) => acc + checksumValue(v), 0);
  if (typeof value === "number") return value;
  if (typeof value === "string") return value.length;
  if (typeof value === "boolean") return value ? 1 : 0;
  if (value != null && typeof value === "object") {
    // Handle quantity {value, unit} from zig wasm
    if (value.value !== undefined) return checksumValue(value.value);
    return 1;
  }
  return 0;
}

// ── fhirpath.js runners ──

function fpjsEvalPreparsed(resource, expr) {
  return checksumValue(fhirpathJs.evaluate(resource, expr, null, fhirpathR4Model));
}

function fpjsEvalFromString(jsonStr, expr) {
  const resource = JSON.parse(jsonStr);
  return checksumValue(fhirpathJs.evaluate(resource, expr, null, fhirpathR4Model));
}

// ── Zig WASM runners ──

function zigEvalWasm(engine, jsonStr, expr) {
  let checksum = 0;
  for (const node of engine.eval({ expr, input: jsonStr, schema: "r5", adapter: "wasm" })) {
    checksum += checksumValue(node.data);
  }
  return checksum;
}

function zigEvalJs(engine, resource, expr) {
  let checksum = 0;
  for (const node of engine.eval({ expr, input: resource, schema: "r5", adapter: "js" })) {
    checksum += checksumValue(node.data);
  }
  return checksum;
}

function zigEvalAuto(engine, resource, expr) {
  // Auto-detect: object input → uses JS adapter automatically
  let checksum = 0;
  for (const node of engine.eval({ expr, input: resource, schema: "r5" })) {
    checksum += checksumValue(node.data);
  }
  return checksum;
}

// ── Benchmark harness ──

function bench(fn, warmup, iterations) {
  let checksum = 0;
  for (let i = 0; i < warmup; i++) fn();
  const start = performance.now();
  for (let i = 0; i < iterations; i++) checksum += fn();
  const elapsed = performance.now() - start;
  return { avgMs: elapsed / iterations, checksum };
}

async function main() {
  const { warmup, iterations, scenario } = parseArgs();

  // Initialize Zig WASM engine
  const wasmBytes = fs.readFileSync(path.join(ROOT, "zig-out/bin/fhirpath.wasm"));
  const modelBytes = fs.readFileSync(path.join(ROOT, "models/r5/model.bin"));
  const engine = await FhirPathEngine.instantiate({
    wasmBytes,
    schemas: [{ name: "r5", prefix: "FHIR", model: modelBytes, isDefault: true }],
  });

  const pad = (s, n) => s.padEnd(n);
  const fmtMs = (ms) => {
    if (ms < 1) return `${(ms * 1000).toFixed(0)} us`.padStart(8);
    return `${ms.toFixed(3)} ms`.padStart(8);
  };

  console.log(`Warmup: ${warmup}, Iterations: ${iterations}\n`);
  console.log(
    pad("Scenario", 50),
    pad("zig-wasm", 14),
    pad("zig-js", 14),
    pad("fhirpath.js", 14),
    pad("fpjs+parse", 14),
    pad("ratio(js/zig)", 14),
  );
  console.log("-".repeat(120));

  const totals = { zig: 0, zigJs: 0, fpjs: 0, fpjsParse: 0, count: 0 };

  for (const sc of scenarios) {
    if (scenario && sc.name !== scenario) continue;

    const filePath = path.resolve(ROOT, sc.file);
    const raw = fs.readFileSync(filePath, "utf8");
    const resource = JSON.parse(raw);

    // Zig WASM: always starts from string JSON (all-in, forced wasm adapter)
    const zigResult = bench(() => zigEvalWasm(engine, raw, sc.expr), warmup, iterations);

    // Zig JS adapter: pre-parsed JS object (forced js adapter)
    const zigJsResult = bench(() => zigEvalJs(engine, resource, sc.expr), warmup, iterations);

    // fhirpath.js: eval-only (pre-parsed object)
    const fpjsResult = bench(() => fpjsEvalPreparsed(resource, sc.expr), warmup, iterations);

    // fhirpath.js: parse+eval (from string, like zig)
    const fpjsParseResult = bench(() => fpjsEvalFromString(raw, sc.expr), warmup, iterations);

    const ratio = fpjsParseResult.avgMs / zigResult.avgMs;

    console.log(
      pad(sc.name, 50),
      fmtMs(zigResult.avgMs),
      "  ",
      fmtMs(zigJsResult.avgMs),
      "  ",
      fmtMs(fpjsResult.avgMs),
      "  ",
      fmtMs(fpjsParseResult.avgMs),
      "  ",
      `${ratio.toFixed(1)}x`.padStart(8),
    );

    // Verify checksums match between all paths
    if (zigResult.checksum !== fpjsResult.checksum) {
      console.log(
        `  ⚠ checksum mismatch: zig=${zigResult.checksum} fpjs=${fpjsResult.checksum}`,
      );
    }
    if (zigResult.checksum !== zigJsResult.checksum) {
      console.log(
        `  ⚠ checksum mismatch: zig=${zigResult.checksum} zigJs=${zigJsResult.checksum}`,
      );
    }

    totals.zig += zigResult.avgMs;
    totals.zigJs += zigJsResult.avgMs;
    totals.fpjs += fpjsResult.avgMs;
    totals.fpjsParse += fpjsParseResult.avgMs;
    totals.count++;
  }

  if (totals.count > 1) {
    console.log("-".repeat(120));
    console.log(
      pad("TOTAL (sum of avgs)", 50),
      fmtMs(totals.zig),
      "  ",
      fmtMs(totals.zigJs),
      "  ",
      fmtMs(totals.fpjs),
      "  ",
      fmtMs(totals.fpjsParse),
      "  ",
      `${(totals.fpjsParse / totals.zig).toFixed(1)}x`.padStart(8),
    );
  }

  engine.dispose();
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
