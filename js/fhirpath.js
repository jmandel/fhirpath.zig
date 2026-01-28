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
  decimal: 3,
  string: 4,
  date: 5,
  time: 6,
  dateTime: 7,
  quantity: 8,
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
  let s = mag.toString();
  if (dec.scale > 0) {
    if (s.length <= dec.scale) {
      s = "0".repeat(dec.scale - s.length + 1) + s;
    }
    const idx = s.length - dec.scale;
    s = s.slice(0, idx) + "." + s.slice(idx);
  }
  if (dec.sign < 0 && mag !== 0n) s = "-" + s;
  return s;
}

export class FhirPathEngine {
  static async instantiate(wasm, options = {}) {
    const resolved = await resolveWasm(wasm);
    const imports = options.imports ?? {};
    const result = await WebAssembly.instantiate(resolved, imports);
    return new FhirPathEngine(result.instance);
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

  registerSchema({ name, prefix = "FHIR", model, isDefault = false }) {
    if (!name) throw new Error("schema name required");
    const nameBuf = this.#writeString(name);
    const prefixBuf = this.#writeString(prefix);
    const modelBuf = this.#writeBytes(toUint8Array(model));

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

  async registerSchemaFromUrl({ name, prefix = "FHIR", url, isDefault = false }) {
    if (!url) throw new Error("schema url required");
    const bytes = await resolveBytes(url);
    this.registerSchema({ name, prefix, model: bytes, isDefault });
  }

  eval({ expr, json, schema = "", env = null, now = undefined } = {}) {
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
    if (json === undefined || json === null) {
      json = "";
    }
    if (expr === undefined || expr === null) {
      expr = "";
    }
    if (schema === undefined || schema === null) {
      schema = "";
    }
    const exprBuf = this.#writeString(expr ?? "");
    const jsonBuf = this.#writeString(json ?? "");
    const schemaBuf = this.#writeString(schema ?? "");

    const status = this.exports.fhirpath_eval(
      this.ctx,
      exprBuf.ptr,
      exprBuf.len,
      jsonBuf.ptr,
      jsonBuf.len,
      schemaBuf.ptr,
      schemaBuf.len,
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
        return this.exports.fhirpath_item_i64(this.ctx, item);
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
    return JSON.parse(text);
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
      this._data = this.engine._decodeValue(this.item);
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
