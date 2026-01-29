import fs from "node:fs";
import path from "node:path";
import { FhirPathEngine } from "../js/fhirpath.js";

const engine = await FhirPathEngine.instantiate({
  wasmBytes: fs.readFileSync(path.resolve("zig-out/bin/fhirpath.wasm")),
  schemas: [{
    name: "r5",
    prefix: "FHIR",
    model: fs.readFileSync(path.resolve("models/r5/model.bin")),
    isDefault: true,
  }],
});

const expr = "name.given";
const input = JSON.stringify({
  resourceType: "Patient",
  name: [{ given: ["Alice", "Bob"] }],
});

const results = [];
for (const node of engine.eval({ expr, json: input, schema: "r5" })) {
  console.log("type_id", node.meta.typeId, "type_name", node.meta.typeName);
  results.push(node.data);
}

console.log("results", results);
engine.dispose();
