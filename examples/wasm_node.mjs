import fs from "node:fs";
import path from "node:path";

const wasmPath = path.resolve("zig-out/bin/fhirpath.wasm");
const wasmBytes = fs.readFileSync(wasmPath);

const { instance } = await WebAssembly.instantiate(wasmBytes, {});
const {
  memory,
  fhirpath_alloc,
  fhirpath_free,
  fhirpath_ctx_new,
  fhirpath_ctx_free,
  fhirpath_ctx_register_schema,
  fhirpath_eval,
  fhirpath_result_iter_new,
  fhirpath_result_iter_next,
  fhirpath_item_data_kind,
  fhirpath_item_data_ptr,
  fhirpath_item_data_len,
  fhirpath_item_type_id,
  fhirpath_type_name_ptr,
  fhirpath_type_name_len,
} = instance.exports;

const encoder = new TextEncoder();
const decoder = new TextDecoder();

function memU8() {
  return new Uint8Array(memory.buffer);
}

function writeBytes(bytes) {
  const ptr = fhirpath_alloc(bytes.length);
  memU8().set(bytes, ptr);
  return { ptr, len: bytes.length };
}

function writeString(s) {
  return writeBytes(encoder.encode(s));
}

const ctx = fhirpath_ctx_new();
if (!ctx) throw new Error("ctx_new failed");

// Register FHIR R5 schema (checked into repo)
const modelBytes = fs.readFileSync(path.resolve("models/r5/model.bin"));
const schemaName = writeString("r5");
const schemaPrefix = writeString("FHIR");
const schemaModel = writeBytes(modelBytes);
const statusSchema = fhirpath_ctx_register_schema(
  ctx,
  schemaName.ptr,
  schemaName.len,
  schemaPrefix.ptr,
  schemaPrefix.len,
  schemaModel.ptr,
  schemaModel.len,
  1,
);
if (statusSchema !== 0) throw new Error(`register_schema failed: ${statusSchema}`);

const expr = "name.given";
const input = JSON.stringify({
  resourceType: "Patient",
  name: [{ given: ["Alice", "Bob"] }],
});

const exprBuf = writeString(expr);
const jsonBuf = writeString(input);
const schemaBuf = writeString("r5");

const statusEval = fhirpath_eval(
  ctx,
  exprBuf.ptr,
  exprBuf.len,
  jsonBuf.ptr,
  jsonBuf.len,
  schemaBuf.ptr,
  schemaBuf.len,
  0,
  0,
);
if (statusEval !== 0) throw new Error(`eval failed: ${statusEval}`);

const iter = fhirpath_result_iter_new(ctx);
const results = [];
for (;;) {
  const item = fhirpath_result_iter_next(ctx, iter);
  if (!item) break;
  const kind = fhirpath_item_data_kind(ctx, item);
  const typeId = fhirpath_item_type_id(ctx, item);
  const typePtr = fhirpath_type_name_ptr(ctx, typeId);
  const typeLen = fhirpath_type_name_len(ctx, typeId);
  const typeName = typePtr ? decoder.decode(memU8().subarray(typePtr, typePtr + typeLen)) : "";
  if (kind === 1) {
    const ptr = fhirpath_item_data_ptr(ctx, item);
    const len = fhirpath_item_data_len(ctx, item);
    const text = decoder.decode(memU8().subarray(ptr, ptr + len));
    results.push(JSON.parse(text));
  }
  const typeIdU32 = typeId >>> 0;
  console.log("type_id", typeIdU32, "type_name", typeName);
}

console.log("results", results);

fhirpath_ctx_free(ctx);

// Optional: free input buffers (keep until after eval)
fhirpath_free(exprBuf.ptr, exprBuf.len);
fhirpath_free(jsonBuf.ptr, jsonBuf.len);
fhirpath_free(schemaBuf.ptr, schemaBuf.len);
fhirpath_free(schemaName.ptr, schemaName.len);
fhirpath_free(schemaPrefix.ptr, schemaPrefix.len);
fhirpath_free(schemaModel.ptr, schemaModel.len);
