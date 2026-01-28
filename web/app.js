import { FhirPathEngine } from "./fhirpath.js";

const exprInput = document.querySelector("#expr");
const jsonInput = document.querySelector("#json");
const runBtn = document.querySelector("#run");
const loadBtn = document.querySelector("#load-example");
const resultsEl = document.querySelector("#results");
const statusEl = document.querySelector("#status");
const inputMetaEl = document.querySelector("#input-meta");
const chipRow = document.querySelector("#chip-row");

const examples = [
  {
    label: "Patient names",
    expr: "name.given",
    json: {
      resourceType: "Patient",
      id: "example",
      name: [
        { family: "Smith", given: ["Alice", "Bob"] },
        { family: "Adams", given: ["Clare"] },
      ],
    },
  },
  {
    label: "Identifier filtering",
    expr: "identifier.where(system = 'http://acme.org/mrn').value",
    json: {
      resourceType: "Patient",
      identifier: [
        { system: "http://acme.org/mrn", value: "A-01492" },
        { system: "http://acme.org/ssn", value: "999-12-1001" },
      ],
    },
  },
  {
    label: "Observation values",
    expr: "valueQuantity.value",
    json: {
      resourceType: "Observation",
      status: "final",
      valueQuantity: { value: 5.4, unit: "mmol/L" },
    },
  },
  {
    label: "Telecom lookup",
    expr: "telecom.where(system = 'phone').value",
    json: {
      resourceType: "Patient",
      telecom: [
        { system: "phone", value: "+1-202-555-0114" },
        { system: "email", value: "hello@example.org" },
      ],
    },
  },
];

function renderChips() {
  chipRow.innerHTML = "";
  examples.forEach((example, idx) => {
    const chip = document.createElement("button");
    chip.className = "chip";
    chip.textContent = example.label;
    chip.addEventListener("click", () => {
      loadExample(idx);
      for (const el of chipRow.children) el.classList.remove("active");
      chip.classList.add("active");
    });
    chipRow.appendChild(chip);
  });
  chipRow.firstChild?.classList.add("active");
}

function loadExample(idx = 0) {
  const example = examples[idx];
  exprInput.value = example.expr;
  jsonInput.value = JSON.stringify(example.json, null, 2);
  updateInputMeta();
}

function updateInputMeta() {
  const text = jsonInput.value || "";
  inputMetaEl.textContent = `${text.length.toLocaleString()} bytes`;
}

function clearResults() {
  resultsEl.innerHTML = "";
}

function addResultCard({ title, body, meta }) {
  const card = document.createElement("div");
  card.className = "result-card";
  const h = document.createElement("h4");
  h.textContent = title;
  const pre = document.createElement("pre");
  pre.textContent = body;
  card.appendChild(h);
  if (meta) {
    const m = document.createElement("div");
    m.className = "meta";
    m.textContent = meta;
    card.appendChild(m);
  }
  card.appendChild(pre);
  resultsEl.appendChild(card);
}

function normalizeDisplay(value) {
  if (value === null) return "null";
  if (typeof value === "string") return value;
  return JSON.stringify(value, null, 2);
}

let engine;

async function init() {
  try {
    statusEl.textContent = "Loading WASM…";
    engine = await FhirPathEngine.instantiate(new URL("./fhirpath.wasm", import.meta.url));

    statusEl.textContent = "Loading schema…";
    await engine.registerSchemaFromUrl({
      name: "r5",
      prefix: "FHIR",
      url: new URL("./model-r5.bin", import.meta.url),
      isDefault: true,
    });

    statusEl.textContent = "Ready";
  } catch (err) {
    statusEl.textContent = "Failed to initialize";
    clearResults();
    addResultCard({
      title: "Init error",
      body: err?.message ?? String(err),
    });
  }
}

async function runExpression() {
  if (!engine) return;
  clearResults();
  statusEl.textContent = "Running…";

  let jsonText = jsonInput.value;
  try {
    const parsed = JSON.parse(jsonText);
    jsonText = JSON.stringify(parsed);
  } catch (err) {
    statusEl.textContent = "Invalid JSON";
    addResultCard({ title: "JSON parse error", body: err?.message ?? String(err) });
    return;
  }

  const expr = exprInput.value.trim();
  if (!expr) {
    statusEl.textContent = "Expression required";
    return;
  }

  try {
    const results = engine.eval({ expr, json: jsonText, schema: "r5", now: new Date() });
    let count = 0;
    for (const node of results) {
      count += 1;
      const meta = `${node.meta.typeName || "(unknown)"} · typeId ${node.meta.typeId}`;
      const value = node.data ?? "<unavailable>";
      addResultCard({
        title: `Result ${count}`,
        body: normalizeDisplay(value),
        meta,
      });
    }
    if (count === 0) {
      addResultCard({ title: "No results", body: "Collection is empty." });
    }
    statusEl.textContent = `Done · ${count} result${count === 1 ? "" : "s"}`;
  } catch (err) {
    statusEl.textContent = "Evaluation error";
    addResultCard({ title: "Eval error", body: err?.message ?? String(err) });
  }
}

renderChips();
loadExample(0);
updateInputMeta();
init();

runBtn.addEventListener("click", runExpression);
loadBtn.addEventListener("click", () => loadExample(0));
jsonInput.addEventListener("input", updateInputMeta);
