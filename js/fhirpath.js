const encoder = new TextEncoder();
const decoder = new TextDecoder();

const DataKind = {
  none: 0,
  nodeRef: 1,
  value: 2,
};

const ValueKind = {
  empty: 0,
  boolean: 1,
  integer: 2,
  long: 3,
  decimal: 4,
  string: 5,
  date: 6,
  time: 7,
  dateTime: 8,
  quantity: 9,
};

const Status = {
  ok: 0,
};

const DECIMAL_SIZE = 28;
const QUANTITY_SIZE = 36;

function toUint8Array(input) {
  if (input == null) return new Uint8Array();
  if (input instanceof Uint8Array) return input;
  if (input instanceof ArrayBuffer) return new Uint8Array(input);
  if (ArrayBuffer.isView(input)) return new Uint8Array(input.buffer, input.byteOffset, input.byteLength);
  throw new Error("Unsupported binary input");
}

async function resolveWasm(wasm) {
  if (wasm instanceof WebAssembly.Module) return wasm;
  if (wasm instanceof Response) return wasm.arrayBuffer();
  if (typeof wasm === "string" || wasm instanceof URL) {
    const res = await fetch(wasm);
    if (!res.ok) throw new Error(`Failed to fetch wasm: ${res.status}`);
    return res.arrayBuffer();
  }
  if (wasm instanceof ArrayBuffer || ArrayBuffer.isView(wasm)) return wasm;
  throw new Error("Unsupported wasm input");
}

async function resolveBytes(input) {
  if (input instanceof Response) return input.arrayBuffer();
  if (typeof input === "string" || input instanceof URL) {
    const res = await fetch(input);
    if (!res.ok) throw new Error(`Failed to fetch bytes: ${res.status}`);
    return res.arrayBuffer();
  }
  if (input instanceof ArrayBuffer || ArrayBuffer.isView(input)) return input;
  throw new Error("Unsupported byte input");
}

function readDecimal(view, ptr) {
  const sign = view.getInt32(ptr + 0, true);
  const scale = view.getUint32(ptr + 4, true);
  const normScale = view.getUint32(ptr + 8, true);
  const mag0 = view.getUint32(ptr + 12, true);
  const mag1 = view.getUint32(ptr + 16, true);
  const mag2 = view.getUint32(ptr + 20, true);
  const mag3 = view.getUint32(ptr + 24, true);
  return { sign, scale, normScale, mag0, mag1, mag2, mag3 };
}

function decimalToString(dec) {
  let mag = BigInt(dec.mag0) |
    (BigInt(dec.mag1) << 32n) |
    (BigInt(dec.mag2) << 64n) |
    (BigInt(dec.mag3) << 96n);
  // Magnitude is stored trimmed (trailing zeros removed), matching normScale.
  // To display with original scale (including trailing zeros), re-inflate magnitude.
  const scale = dec.scale;
  if (dec.normScale < scale) {
    const pad = scale - dec.normScale;
    for (let i = 0; i < pad; i++) mag *= 10n;
  }
  let s = mag.toString();
  if (scale > 0) {
    if (s.length <= scale) {
      s = "0".repeat(scale - s.length + 1) + s;
    }
    const idx = s.length - scale;
    s = s.slice(0, idx) + "." + s.slice(idx);
  }
  if (dec.sign < 0 && mag !== 0n) s = "-" + s;
  return s;
}

// ── JS Node Map for cross-WASM JS object navigation ──

class JsNodeMap {
  constructor() {
    this.map = new Map();
    this.nextId = 1; // 0 is reserved for "not found"
    this.memory = null;
  }

  setRoot(value) {
    this.map.clear();
    this.nextId = 1;
    return this.intern(value);
  }

  intern(value) {
    const id = this.nextId++;
    this.map.set(id, value);
    return id;
  }

  get(id) {
    return this.map.get(id);
  }

  readString(ptr, len) {
    if (!len) return "";
    const bytes = new Uint8Array(this.memory.buffer, ptr, len);
    return decoder.decode(bytes);
  }

  writeString(str, ptr, maxLen) {
    const encoded = encoder.encode(str);
    const len = Math.min(encoded.length, maxLen);
    const target = new Uint8Array(this.memory.buffer, ptr, len);
    target.set(encoded.subarray(0, len));
    return len;
  }
}

function createJsAdapterImports(nodeMap) {
  return {
    env: {
      js_node_kind(ref) {
        const val = nodeMap.get(ref);
        if (val === null || val === undefined) return 5; // null
        if (Array.isArray(val)) return 1; // array
        const t = typeof val;
        if (t === "string") return 2; // string
        if (t === "number") return 3; // number
        if (t === "boolean") return 4; // boolean
        // Decimal wrapper: object with numeric valueOf() (e.g. JsonDecimal)
        if (t === "object" && typeof val.valueOf() === "number" && val.valueOf() !== val) return 3;
        if (t === "object") return 0; // object
        return 5; // null
      },

      js_object_get(ref, keyPtr, keyLen) {
        const obj = nodeMap.get(ref);
        if (obj === null || obj === undefined || typeof obj !== "object" || Array.isArray(obj)) return 0;
        const key = nodeMap.readString(keyPtr, keyLen);
        if (!Object.prototype.hasOwnProperty.call(obj, key)) return 0;
        const child = obj[key];
        return nodeMap.intern(child);
      },

      js_object_count(ref) {
        const obj = nodeMap.get(ref);
        if (obj === null || obj === undefined || typeof obj !== "object" || Array.isArray(obj)) return 0;
        return Object.keys(obj).length;
      },

      js_object_keys(ref, outPtr, maxPairs) {
        const obj = nodeMap.get(ref);
        if (obj === null || obj === undefined || typeof obj !== "object" || Array.isArray(obj)) return 0;
        const keys = Object.keys(obj);
        const count = Math.min(keys.length, maxPairs);
        const out = new Uint32Array(nodeMap.memory.buffer, outPtr, count * 2);
        for (let i = 0; i < count; i++) {
          const key = keys[i];
          out[i * 2] = nodeMap.intern(key); // key as a "string node"
          out[i * 2 + 1] = nodeMap.intern(obj[key]); // value node
        }
        return count;
      },

      js_array_len(ref) {
        const arr = nodeMap.get(ref);
        if (!Array.isArray(arr)) return 0;
        return arr.length;
      },

      js_array_at(ref, idx) {
        const arr = nodeMap.get(ref);
        if (!Array.isArray(arr) || idx >= arr.length) return 0;
        return nodeMap.intern(arr[idx]);
      },

      js_string(ref, outPtr, maxLen) {
        const val = nodeMap.get(ref);
        if (typeof val !== "string") return 0;
        return nodeMap.writeString(val, outPtr, maxLen);
      },

      js_number_text(ref, outPtr, maxLen) {
        const val = nodeMap.get(ref);
        if (typeof val === "number") {
          const text = Object.is(val, -0) ? "-0" : String(val);
          return nodeMap.writeString(text, outPtr, maxLen);
        }
        // Decimal wrapper: use toString() which preserves original precision
        if (typeof val === "object" && val !== null && typeof val.valueOf() === "number" && val.valueOf() !== val) {
          return nodeMap.writeString(String(val), outPtr, maxLen);
        }
        return 0;
      },

      js_boolean(ref) {
        const val = nodeMap.get(ref);
        return val === true ? 1 : 0;
      },
    },
  };
}

export class FhirPathEngine {
  static async instantiate(options = {}) {
    const wasmSource = options.wasmModule ?? options.wasmBytes ?? options.wasmUrl
      ?? new URL("./fhirpath.wasm", import.meta.url);
    const resolved = await resolveWasm(wasmSource);
    const nodeMap = new JsNodeMap();
    const jsImports = createJsAdapterImports(nodeMap);
    const userImports = options.imports ?? {};
    // Merge JS adapter imports with user-provided imports
    const mergedImports = { ...jsImports };
    for (const [ns, fns] of Object.entries(userImports)) {
      mergedImports[ns] = { ...(mergedImports[ns] ?? {}), ...fns };
    }
    const result = await WebAssembly.instantiate(resolved, mergedImports);
    const engine = new FhirPathEngine(result.instance);
    nodeMap.memory = engine.memory;
    engine._nodeMap = nodeMap;
    engine.jsonParser = options.jsonParser ?? JSON.parse;

    if (options.schemas) {
      for (const schema of options.schemas) {
        await engine.registerSchema(schema);
      }
    }

    return engine;
  }

  constructor(instance) {
    this.instance = instance;
    this.exports = instance.exports;
    this.memory = this.exports.memory;
    this.ctx = this.exports.fhirpath_ctx_new();
    if (!this.ctx) throw new Error("fhirpath_ctx_new failed");
    this._lastJsonBuf = null;
  }

  dispose() {
    if (this.ctx) {
      this.exports.fhirpath_ctx_free(this.ctx);
      this.ctx = 0;
    }
    if (this._lastJsonBuf) {
      this.#free(this._lastJsonBuf);
      this._lastJsonBuf = null;
    }
  }

  async registerSchema({ name, prefix = "FHIR", url, model, isDefault = false }) {
    if (!name) throw new Error("schema name required");
    let bytes;
    if (model) {
      bytes = toUint8Array(model);
    } else {
      const resolvedUrl = url ?? new URL(`./model-${name}.bin`, import.meta.url);
      bytes = toUint8Array(await resolveBytes(resolvedUrl));
    }

    const nameBuf = this.#writeString(name);
    const prefixBuf = this.#writeString(prefix);
    const modelBuf = this.#writeBytes(bytes);

    const status = this.exports.fhirpath_ctx_register_schema(
      this.ctx,
      nameBuf.ptr,
      nameBuf.len,
      prefixBuf.ptr,
      prefixBuf.len,
      modelBuf.ptr,
      modelBuf.len,
      isDefault ? 1 : 0,
    );

    this.#free(nameBuf);
    this.#free(prefixBuf);
    this.#free(modelBuf);

    if (status !== Status.ok) {
      throw new Error(`register_schema failed: ${status}`);
    }
  }

  /**
   * Evaluate a FHIRPath expression against input data.
   *
   * Input can be provided as:
   * - `json` (string): JSON text, always parsed inside WASM (legacy parameter)
   * - `input` (string | object): auto-detected — strings are parsed inside WASM,
   *   objects are navigated via JS adapter callbacks
   *
   * The `adapter` option overrides auto-detection of where the parsed tree lives:
   * - `"wasm"`: copy input into WASM linear memory and parse there (even if input is
   *   an object — serializes it first)
   * - `"js"`: keep the parsed tree on the JS side and navigate it via WASM import
   *   callbacks (even if input is a string — calls JSON.parse first). This is faster
   *   because it skips the WASM-side parse and only visits nodes the evaluator touches.
   *
   * The `fhir` option controls FHIR-aware traversal (field/_field primitive merging,
   * underscore key hiding, resourceType suppression). Defaults to `true`. Set to `false`
   * for plain JSON traversal without FHIR semantics.
   *
   * The `json` parameter is provided for backward compatibility and always uses the
   * `"wasm"` adapter (unless `adapter: "js"` is explicitly set).
   */
  eval({ expr, json, input, schema = "", env = null, now = undefined, adapter, fhir = true } = {}) {
    if (env && Object.keys(env).length > 0) {
      throw new Error("env is not supported by the current wasm build");
    }
    if (now !== undefined) {
      if (now instanceof Date) {
        this.setNowDate(now);
      } else {
        this.setNowEpochSeconds(now);
      }
    }
    expr = expr ?? "";
    schema = schema ?? "";

    // Resolve the actual input and adapter
    const rawInput = json !== undefined ? json : input;
    let resolvedAdapter = adapter;
    if (!resolvedAdapter) {
      if (json !== undefined) {
        // Legacy `json` param: default to wasm unless explicitly overridden
        resolvedAdapter = "wasm";
      } else if (typeof rawInput === "string") {
        resolvedAdapter = "wasm";
      } else {
        resolvedAdapter = "js";
      }
    }

    const fhirFlag = fhir ? 1 : 0;

    if (resolvedAdapter === "js") {
      // JS adapter path: navigate pre-parsed JS object via WASM imports
      let resource = rawInput;
      if (typeof resource === "string") {
        resource = JSON.parse(resource);
      }
      return this.#evalJs(expr, resource ?? null, schema, fhirFlag);
    } else {
      // WASM path: copy JSON text into WASM, parse with std.json
      let jsonStr = rawInput;
      if (typeof jsonStr !== "string") {
        jsonStr = jsonStr != null ? JSON.stringify(jsonStr) : "";
      }
      return this.#evalWasm(expr, jsonStr ?? "", schema, fhirFlag);
    }
  }

  #evalWasm(expr, json, schema, fhirFlag) {
    const exprBuf = this.#writeString(expr);
    const jsonBuf = this.#writeString(json);
    const schemaBuf = this.#writeString(schema);

    const status = this.exports.fhirpath_eval(
      this.ctx,
      exprBuf.ptr,
      exprBuf.len,
      jsonBuf.ptr,
      jsonBuf.len,
      schemaBuf.ptr,
      schemaBuf.len,
      fhirFlag,
      0,
      0,
    );

    this.#free(exprBuf);
    this.#free(schemaBuf);

    if (status !== Status.ok) {
      this.#free(jsonBuf);
      throw new Error(`eval failed: ${status}`);
    }

    if (this._lastJsonBuf) {
      this.#free(this._lastJsonBuf);
    }
    this._lastJsonBuf = jsonBuf;

    return new FhirPathResult(this);
  }

  #evalJs(expr, resource, schema, fhirFlag) {
    const rootId = this._nodeMap.setRoot(resource);
    const exprBuf = this.#writeString(expr);
    const schemaBuf = this.#writeString(schema);

    const status = this.exports.fhirpath_eval_js(
      this.ctx,
      exprBuf.ptr,
      exprBuf.len,
      rootId,
      schemaBuf.ptr,
      schemaBuf.len,
      fhirFlag,
      0,
      0,
    );

    this.#free(exprBuf);
    this.#free(schemaBuf);

    if (this._lastJsonBuf) {
      this.#free(this._lastJsonBuf);
      this._lastJsonBuf = null;
    }

    if (status !== Status.ok) {
      throw new Error(`eval_js failed: ${status}`);
    }

    return new FhirPathResult(this);
  }

  evalXml({ expr, xml, schema = "", env = null, now = undefined } = {}) {
    if (env && Object.keys(env).length > 0) {
      throw new Error("env is not supported by the current wasm build");
    }
    if (now !== undefined) {
      if (now instanceof Date) {
        this.setNowDate(now);
      } else {
        this.setNowEpochSeconds(now);
      }
    }
    if (xml === undefined || xml === null) {
      xml = "";
    }
    if (expr === undefined || expr === null) {
      expr = "";
    }
    if (schema === undefined || schema === null) {
      schema = "";
    }
    const exprBuf = this.#writeString(expr);
    const xmlBuf = this.#writeString(xml);
    const schemaBuf = this.#writeString(schema);

    const status = this.exports.fhirpath_eval_xml(
      this.ctx,
      exprBuf.ptr,
      exprBuf.len,
      xmlBuf.ptr,
      xmlBuf.len,
      schemaBuf.ptr,
      schemaBuf.len,
      0,
      0,
    );

    this.#free(exprBuf);
    this.#free(schemaBuf);

    if (status !== Status.ok) {
      this.#free(xmlBuf);
      throw new Error(`eval_xml failed: ${status}`);
    }

    if (this._lastJsonBuf) {
      this.#free(this._lastJsonBuf);
    }
    this._lastJsonBuf = xmlBuf;

    return new FhirPathResult(this);
  }

  setNowEpochSeconds(seconds) {
    const value = typeof seconds === "bigint" ? seconds : BigInt(Math.floor(seconds));
    const status = this.exports.fhirpath_ctx_set_time(this.ctx, value);
    if (status !== Status.ok) {
      throw new Error(`set_time failed: ${status}`);
    }
  }

  setNowDate(date) {
    const ms = date instanceof Date ? date.getTime() : Number(date);
    this.setNowEpochSeconds(Math.floor(ms / 1000));
  }

  typeNameFromId(typeId) {
    const ptr = this.exports.fhirpath_type_name_ptr(this.ctx, typeId >>> 0);
    const len = this.exports.fhirpath_type_name_len(this.ctx, typeId >>> 0);
    if (!ptr || !len) return "";
    return decoder.decode(this.#mem().subarray(ptr, ptr + len));
  }

  _decodeValue(item) {
    const kind = this.exports.fhirpath_item_value_kind(this.ctx, item);
    switch (kind) {
      case ValueKind.empty:
        return null;
      case ValueKind.boolean:
        return !!this.exports.fhirpath_item_bool(this.ctx, item);
      case ValueKind.integer:
      case ValueKind.long: {
        const bigVal = this.exports.fhirpath_item_i64(this.ctx, item);
        if (typeof bigVal === "bigint") {
          if (bigVal >= -9007199254740991n && bigVal <= 9007199254740991n) return Number(bigVal);
          return bigVal;
        }
        return bigVal;
      }
      case ValueKind.decimal:
        return this.#readDecimalValue(item);
      case ValueKind.string:
      case ValueKind.date:
      case ValueKind.time:
      case ValueKind.dateTime:
        return this.#readStringValue(item);
      case ValueKind.quantity:
        return this.#readQuantityValue(item);
      default:
        return null;
    }
  }

  _readNodeJson(item) {
    const ptr = this.exports.fhirpath_item_data_ptr(this.ctx, item);
    const len = this.exports.fhirpath_item_data_len(this.ctx, item);
    if (!ptr || !len) return null;
    const text = decoder.decode(this.#mem().subarray(ptr, ptr + len));
    return this.jsonParser(text);
  }

  _readSourceSpan(item) {
    const pos = this.exports.fhirpath_item_source_pos(this.ctx, item);
    const end = this.exports.fhirpath_item_source_end(this.ctx, item);
    return { pos, end };
  }

  #readStringValue(item) {
    const ptr = this.exports.fhirpath_item_str_ptr(this.ctx, item);
    const len = this.exports.fhirpath_item_str_len(this.ctx, item);
    if (!ptr || !len) return "";
    return decoder.decode(this.#mem().subarray(ptr, ptr + len));
  }

  #readDecimalValue(item) {
    const buf = this.#alloc(DECIMAL_SIZE);
    const status = this.exports.fhirpath_item_decimal(this.ctx, item, buf.ptr);
    if (status !== Status.ok) {
      this.#free(buf);
      return null;
    }
    const view = new DataView(this.#mem().buffer);
    const dec = readDecimal(view, buf.ptr);
    this.#free(buf);
    return decimalToString(dec);
  }

  #readQuantityValue(item) {
    const buf = this.#alloc(QUANTITY_SIZE);
    const status = this.exports.fhirpath_item_quantity(this.ctx, item, buf.ptr);
    if (status !== Status.ok) {
      this.#free(buf);
      return null;
    }
    const view = new DataView(this.#mem().buffer);
    const dec = readDecimal(view, buf.ptr);
    const unitPtr = view.getUint32(buf.ptr + DECIMAL_SIZE, true);
    const unitLen = view.getUint32(buf.ptr + DECIMAL_SIZE + 4, true);
    const unit = unitPtr && unitLen ? decoder.decode(this.#mem().subarray(unitPtr, unitPtr + unitLen)) : "";
    this.#free(buf);
    return { value: decimalToString(dec), unit };
  }

  #mem() {
    return new Uint8Array(this.memory.buffer);
  }

  #alloc(len) {
    const ptr = this.exports.fhirpath_alloc(len);
    if (!ptr) throw new Error("alloc failed");
    return { ptr, len };
  }

  #free(buf) {
    if (buf && buf.ptr && buf.len) this.exports.fhirpath_free(buf.ptr, buf.len);
  }

  #writeBytes(bytes) {
    if (bytes.length === 0) return { ptr: 0, len: 0 };
    const buf = this.#alloc(bytes.length);
    this.#mem().set(bytes, buf.ptr);
    return buf;
  }

  #writeString(str) {
    return this.#writeBytes(encoder.encode(str));
  }
}

export class FhirPathResult {
  constructor(engine) {
    this.engine = engine;
  }

  [Symbol.iterator]() {
    const engine = this.engine;
    const iter = engine.exports.fhirpath_result_iter_new(engine.ctx);
    return {
      next() {
        const handle = engine.exports.fhirpath_result_iter_next(engine.ctx, iter);
        if (!handle) return { done: true };
        return { done: false, value: new FhirPathNode(engine, handle) };
      },
    };
  }
}

export class FhirPathNode {
  constructor(engine, itemHandle) {
    this.engine = engine;
    this.item = itemHandle;
    this._data = undefined;
    this._meta = undefined;
  }

  get data() {
    if (this._data !== undefined) return this._data;
    const kind = this.engine.exports.fhirpath_item_data_kind(this.engine.ctx, this.item);
    if (kind === DataKind.nodeRef) {
      this._data = this.engine._readNodeJson(this.item);
      return this._data;
    }
    if (kind === DataKind.value) {
      this._data = this.engine._decodeValue(this.item) ?? this.engine._readNodeJson(this.item);
      return this._data;
    }
    this._data = null;
    return this._data;
  }

  get meta() {
    if (this._meta) return this._meta;
    const typeId = this.engine.exports.fhirpath_item_type_id(this.engine.ctx, this.item) >>> 0;
    this._meta = {
      typeId,
      typeName: this.engine.typeNameFromId(typeId),
      source: this.engine._readSourceSpan(this.item),
    };
    return this._meta;
  }
}
