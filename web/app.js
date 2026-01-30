import { FhirPathEngine } from "./fhirpath.js";

const exprInput = document.querySelector("#expr");
const jsonInput = document.querySelector("#json");
const resultsEl = document.querySelector("#results");
const statusEl = document.querySelector("#status");
const inputMetaEl = document.querySelector("#input-meta");
const inputLabel = document.querySelector("#input-label");
const chipRow = document.querySelector("#chip-row");
const schemaSelect = document.querySelector("#schema-select");
const formatToggle = document.querySelector("#format-toggle");

let currentFormat = "json-string";

const examples = [
  {
    label: "Names",
    expr: "// Extract all given names\nname.given",
    json: {
      resourceType: "Patient",
      id: "example",
      name: [
        { family: "Smith", given: ["Alice", "Bob"] },
        { family: "Adams", given: ["Clare"] },
      ],
    },
    xml: `<Patient>
  <id value="example"/>
  <name>
    <family value="Smith"/>
    <given value="Alice"/>
    <given value="Bob"/>
  </name>
  <name>
    <family value="Adams"/>
    <given value="Clare"/>
  </name>
</Patient>`,
  },
  {
    label: "Where",
    expr: "// Filter identifiers by system\nidentifier\n  .where(system = 'http://acme.org/mrn')\n  .value",
    json: {
      resourceType: "Patient",
      identifier: [
        { system: "http://acme.org/mrn", value: "A-01492" },
        { system: "http://acme.org/ssn", value: "999-12-1001" },
      ],
    },
    xml: `<Patient>
  <identifier>
    <system value="http://acme.org/mrn"/>
    <value value="A-01492"/>
  </identifier>
  <identifier>
    <system value="http://acme.org/ssn"/>
    <value value="999-12-1001"/>
  </identifier>
</Patient>`,
  },
  {
    label: "Quantity",
    expr: "// Access a Quantity value\nvalue.ofType(Quantity)",
    json: {
      resourceType: "Observation",
      status: "final",
      valueQuantity: {
        value: 5.4,
        unit: "mmol/L",
        system: "http://unitsofmeasure.org",
        code: "mmol/L",
      },
    },
    xml: `<Observation>
  <status value="final"/>
  <valueQuantity>
    <value value="5.4"/>
    <unit value="mmol/L"/>
    <system value="http://unitsofmeasure.org"/>
    <code value="mmol/L"/>
  </valueQuantity>
</Observation>`,
  },
  {
    label: "Telecom",
    expr: "// Find phone numbers\ntelecom\n  .where(system = 'phone')\n  .value",
    json: {
      resourceType: "Patient",
      telecom: [
        { system: "phone", value: "+1-202-555-0114" },
        { system: "email", value: "hello@example.org" },
      ],
    },
    xml: `<Patient>
  <telecom>
    <system value="phone"/>
    <value value="+1-202-555-0114"/>
  </telecom>
  <telecom>
    <system value="email"/>
    <value value="hello@example.org"/>
  </telecom>
</Patient>`,
  },
  {
    label: "Types",
    expr: "// Reflect on element types\nbirthDate.type() |\n  active.type() |\n  gender.type()",
    json: {
      resourceType: "Patient",
      active: true,
      birthDate: "1974-12-25",
      gender: "male",
    },
    xml: `<Patient>
  <active value="true"/>
  <birthDate value="1974-12-25"/>
  <gender value="male"/>
</Patient>`,
  },
  {
    label: "Exists",
    expr: "// Check element existence\nname.where(family.exists()).count()",
    json: {
      resourceType: "Patient",
      name: [
        { family: "Smith", given: ["Alice"] },
        { given: ["Bob"] },
      ],
    },
    xml: `<Patient>
  <name>
    <family value="Smith"/>
    <given value="Alice"/>
  </name>
  <name>
    <given value="Bob"/>
  </name>
</Patient>`,
  },
  {
    label: "Math",
    expr: "// Arithmetic on quantity values\nvalue.ofType(Quantity).value * 2",
    json: {
      resourceType: "Observation",
      status: "final",
      valueQuantity: { value: 98.6, unit: "degF" },
    },
    xml: `<Observation>
  <status value="final"/>
  <valueQuantity>
    <value value="98.6"/>
    <unit value="degF"/>
  </valueQuantity>
</Observation>`,
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
      runExpression();
    });
    chipRow.appendChild(chip);
  });
  chipRow.firstChild?.classList.add("active");
}

let currentExampleIdx = 0;

function loadExample(idx = 0) {
  currentExampleIdx = idx;
  const example = examples[idx];
  exprInput.value = example.expr;
  if (currentFormat === "xml") {
    jsonInput.value = example.xml;
  } else {
    jsonInput.value = JSON.stringify(example.json, null, 2);
  }
  inputLabel.textContent = currentFormat === "xml" ? "Input XML" : "Input JSON";
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
    statusEl.textContent = "Loading WASM + schemas…";
    engine = await FhirPathEngine.instantiate();

    const availableSchemas = [];
    for (const s of [{ name: "r5", isDefault: true }, { name: "r4" }]) {
      try {
        await engine.registerSchema(s);
        availableSchemas.push(s.name);
      } catch (err) {
        console.warn(`${s.name} model not available`, err);
      }
    }
    if (!availableSchemas.includes(schemaSelect.value)) {
      schemaSelect.value = availableSchemas[0];
    }

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

  const inputText = jsonInput.value;
  const expr = exprInput.value.trim();
  if (!expr) {
    statusEl.textContent = "Expression required";
    return;
  }

  const selectedSchema = schemaSelect.value || "r5";

  if (currentFormat === "json-string") {
    // WASM-side parse: numbers stay as strings — full precision
    try {
      const t0 = performance.now();
      const results = engine.eval({ expr, input: inputText, adapter: "wasm", schema: selectedSchema, now: new Date() });
      renderResults(results, t0);
    } catch (err) {
      statusEl.textContent = "Evaluation error";
      addResultCard({ title: "Eval error", body: err?.message ?? String(err) });
    }
  } else if (currentFormat === "json-adapter") {
    // JS adapter: fast, but numbers go through V8 f64
    let parsed;
    try {
      parsed = JSON.parse(inputText);
    } catch (err) {
      statusEl.textContent = "Invalid JSON";
      addResultCard({ title: "JSON parse error", body: err?.message ?? String(err) });
      return;
    }

    try {
      const t0 = performance.now();
      const results = engine.eval({ expr, input: parsed, schema: selectedSchema, now: new Date() });
      renderResults(results, t0);
    } catch (err) {
      statusEl.textContent = "Evaluation error";
      addResultCard({ title: "Eval error", body: err?.message ?? String(err) });
    }
  } else {
    // XML string: parsed inside WASM
    try {
      const t0 = performance.now();
      const results = engine.evalXml({ expr, xml: inputText, schema: selectedSchema, now: new Date() });
      renderResults(results, t0);
    } catch (err) {
      statusEl.textContent = "Evaluation error";
      addResultCard({ title: "Eval error", body: err?.message ?? String(err) });
    }
  }
}

function renderResults(results, t0) {
  let count = 0;
  for (const node of results) {
    count += 1;
    const meta = `${node.meta.typeName || "(unknown)"}`;
    const value = node.data ?? "<unavailable>";
    addResultCard({
      title: `Result ${count}`,
      body: normalizeDisplay(value),
      meta,
    });
  }
  const elapsedMs = performance.now() - t0;
  if (count === 0) {
    addResultCard({ title: "No results", body: "Collection is empty." });
  }
  const timeStr = elapsedMs < 1 ? `${(elapsedMs * 1000).toFixed(0)} µs` : `${elapsedMs.toFixed(1)} ms`;
  statusEl.textContent = `${count} result${count === 1 ? "" : "s"} · ${timeStr}`;
}

renderChips();
loadExample(0);
updateInputMeta();
init().then(() => {
  // Auto-run once initialized so the results panel isn't empty on load.
  runExpression();
});

let rerunTimer = null;
function scheduleRun() {
  if (rerunTimer) clearTimeout(rerunTimer);
  rerunTimer = setTimeout(() => {
    runExpression();
    rerunTimer = null;
  }, 30);
}

exprInput.addEventListener("input", scheduleRun);
jsonInput.addEventListener("input", () => {
  updateInputMeta();
  scheduleRun();
});
schemaSelect.addEventListener("change", runExpression);
formatToggle.addEventListener("click", (e) => {
  const btn = e.target.closest(".toggle-btn");
  if (!btn) return;
  const fmt = btn.dataset.format;
  if (fmt === currentFormat) return;
  currentFormat = fmt;
  for (const el of formatToggle.querySelectorAll(".toggle-btn")) {
    el.classList.toggle("active", el.dataset.format === fmt);
  }
  loadExample(currentExampleIdx);
  runExpression();
});
