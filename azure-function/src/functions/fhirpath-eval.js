import { app } from "@azure/functions";
import { FhirPathEngine } from "../../fhirpath.js";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const FUNCTION_ROOT = path.resolve(__dirname, "../..");

const EVALUATOR_NAME = "fhirpath.zig";

const ALLOWED_ORIGINS = [
  "https://fhirpath-lab.com",
  "https://dev.fhirpath-lab.com",
  "https://hackweek.fhirpath-lab.com",
  "http://localhost:3000",
];

// Singleton engine instances keyed by schema name, initialised once
const engines = {};
let engineInitPromise = null;

async function initEngines() {
  if (engineInitPromise) return engineInitPromise;
  engineInitPromise = (async () => {
    const wasmPath = path.join(FUNCTION_ROOT, "fhirpath.wasm");
    if (!fs.existsSync(wasmPath)) {
      throw new Error(
        "fhirpath.wasm not found. Run scripts/build_azure_function.sh first."
      );
    }
    const wasmBytes = fs.readFileSync(wasmPath);

    for (const schemaName of ["r5", "r4"]) {
      const modelPath = path.join(FUNCTION_ROOT, `model-${schemaName}.bin`);
      if (!fs.existsSync(modelPath)) {
        console.warn(
          `model-${schemaName}.bin not found – schema "${schemaName}" will not be available`
        );
        continue;
      }
      const modelBytes = fs.readFileSync(modelPath);
      try {
        const engine = await FhirPathEngine.instantiate({
          wasmBytes,
          schemas: [
            {
              name: schemaName,
              prefix: "FHIR",
              model: modelBytes,
              isDefault: true,
            },
          ],
        });
        engines[schemaName] = engine;
        console.log(`Loaded fhirpath.zig engine with schema: ${schemaName}`);
      } catch (err) {
        console.error(
          `Failed to initialise engine for schema "${schemaName}":`,
          err
        );
      }
    }
  })();
  return engineInitPromise;
}

function getCorsHeaders(origin) {
  if (ALLOWED_ORIGINS.includes(origin)) {
    return {
      "Access-Control-Allow-Origin": origin,
      "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
      "Access-Control-Allow-Headers": "Content-Type, Accept",
      "Access-Control-Max-Age": "86400",
    };
  }
  return {};
}

function createOperationOutcome(severity, code, diagnostics) {
  return {
    resourceType: "OperationOutcome",
    issue: [{ severity, code, diagnostics }],
  };
}

/**
 * Map a fhirpath.zig result node to a FHIR ParametersParameter part.
 * Returns the part object with name and value[x] / extension set appropriately.
 *
 * @param {string} typeName  - FHIRPath System type (e.g. "System.String")
 * @param {*}      data      - the result value
 * @param {string} fhirTypeName - original FHIR type (e.g. "FHIR.code"), may be empty
 */
function resultNodeToPart(typeName, data, fhirTypeName) {
  // Extract the local FHIR type name (e.g. "FHIR.code" → "code")
  const fhirLocal = fhirTypeName?.includes(".") ? fhirTypeName.split(".").pop() : fhirTypeName;

  // Use the FHIR type when it is a more specific primitive that has its own value[x]
  const fhirPrimitiveTypes = new Set([
    "code", "uri", "oid", "id", "markdown", "base64Binary", "canonical",
    "url", "xhtml", "date", "dateTime", "instant", "time",
    "integer", "positiveInt", "unsignedInt", "integer64",
  ]);
  const name = (fhirLocal && fhirPrimitiveTypes.has(fhirLocal)) ? fhirLocal : (typeName || "string");
  const part = { name };

  if (data === null || data === undefined) {
    part.extension = [
      {
        url: "http://fhir.forms-lab.com/StructureDefinition/json-value",
        valueString: "null",
      },
    ];
    return part;
  }

  switch (name) {
    case "string":
    case "System.String":
    case "markdown":
    case "id":
    case "oid":
    case "xhtml":
    case "base64Binary":
    case "canonical":
    case "url":
      part.name = name === "System.String" ? "string" : name;
      if (data === "") {
        part.name = "empty-string";
        part.valueString = "";
      } else {
        part.valueString = String(data);
      }
      break;

    case "uri":
      part.valueUri = String(data);
      break;

    case "boolean":
    case "System.Boolean":
      part.name = "boolean";
      part.valueBoolean = Boolean(data);
      break;

    case "integer":
    case "System.Integer":
    case "positiveInt":
    case "unsignedInt":
      part.name = name === "System.Integer" ? "integer" : name;
      part.valueInteger = Number(data);
      break;

    case "integer64":
      part.valueInteger64 = String(data);
      break;

    case "decimal":
    case "System.Decimal":
      part.name = "decimal";
      // JsonDecimal has valueOf(); use the numeric value
      part.valueDecimal =
        typeof data === "object" && data !== null ? data.valueOf() : Number(data);
      break;

    case "date":
    case "System.Date":
      part.name = "date";
      part.valueDate = String(data);
      break;

    case "dateTime":
    case "System.DateTime":
      part.name = "dateTime";
      part.valueDateTime = String(data);
      break;

    case "instant":
      part.valueInstant = String(data);
      break;

    case "time":
    case "System.Time":
      part.name = "time";
      part.valueTime = String(data);
      break;

    case "code":
      part.valueCode = String(data);
      break;

    case "Quantity":
    case "SimpleQuantity":
    case "MoneyQuantity":
    case "Age":
    case "Distance":
    case "Duration":
    case "Count":
      part.valueQuantity =
        typeof data === "object" && "value" in data
          ? data
          : { value: Number(data) };
      break;

    case "HumanName":
      part.valueHumanName = data;
      break;

    case "Address":
      part.valueAddress = data;
      break;

    case "ContactPoint":
      part.valueContactPoint = data;
      break;

    case "Coding":
      part.valueCoding = data;
      break;

    case "CodeableConcept":
      part.valueCodeableConcept = data;
      break;

    case "Identifier":
      part.valueIdentifier = data;
      break;

    case "Period":
      part.valuePeriod = data;
      break;

    case "Range":
      part.valueRange = data;
      break;

    case "Ratio":
      part.valueRatio = data;
      break;

    case "Reference":
      part.valueReference = data;
      break;

    case "Annotation":
      part.valueAnnotation = data;
      break;

    case "Attachment":
      part.valueAttachment = data;
      break;

    case "SampledData":
      part.valueSampledData = data;
      break;

    case "Signature":
      part.valueSignature = data;
      break;

    case "Timing":
      part.valueTiming = data;
      break;

    case "Meta":
      part.valueMeta = data;
      break;

    default:
      // FHIR resource type or unknown complex type
      if (
        data !== null &&
        typeof data === "object" &&
        typeof data.resourceType === "string"
      ) {
        part.resource = data;
      } else if (typeof data === "string") {
        part.valueString = data;
      } else {
        // Backbone element or other non-representable type
        part.extension = [
          {
            url: "http://fhir.forms-lab.com/StructureDefinition/json-value",
            valueString: JSON.stringify(data, null, 2),
          },
        ];
      }
      break;
  }

  return part;
}

/**
 * Evaluate a FHIRPath expression and collect result parts.
 * @param {FhirPathEngine} engine
 * @param {string} expr
 * @param {string} resourceJson - serialised FHIR resource / context item
 * @param {string} schema - schema name (e.g. "r5")
 * @returns {Array} array of ParametersParameter parts
 */
function evaluateAndCollect(engine, expr, resourceJson, schema) {
  const parts = [];
  for (const node of engine.eval({
    expr,
    input: resourceJson,
    schema,
    adapter: "wasm",
    now: new Date(),
  })) {
    parts.push(resultNodeToPart(node.meta.typeName, node.data, node.meta.fhirTypeName));
  }
  return parts;
}

/**
 * Main request handler for both /$fhirpath-r5 and /$fhirpath-r4 endpoints.
 */
async function handleFhirPathRequest(request, _context, schema) {
  const origin = request.headers.get("origin") ?? "";
  const corsHeaders = getCorsHeaders(origin);

  // Handle CORS preflight
  if (request.method === "OPTIONS") {
    return { status: 204, headers: corsHeaders };
  }

  // Ensure engines are initialised
  try {
    await initEngines();
  } catch (err) {
    return {
      status: 503,
      headers: { "Content-Type": "application/fhir+json", ...corsHeaders },
      jsonBody: createOperationOutcome(
        "fatal",
        "exception",
        `Engine initialisation failed: ${err.message}`
      ),
    };
  }

  const engine = engines[schema];
  if (!engine) {
    return {
      status: 503,
      headers: { "Content-Type": "application/fhir+json", ...corsHeaders },
      jsonBody: createOperationOutcome(
        "error",
        "not-supported",
        `Schema "${schema}" is not available. ` +
          `Ensure model-${schema}.bin exists and re-run scripts/build_azure_function.sh.`
      ),
    };
  }

  // Parse request body
  let inputParameters;
  try {
    inputParameters = await request.json();
  } catch (_err) {
    return {
      status: 400,
      headers: { "Content-Type": "application/fhir+json", ...corsHeaders },
      jsonBody: createOperationOutcome(
        "error",
        "invalid",
        "Request body is not valid JSON"
      ),
    };
  }

  if (inputParameters.resourceType !== "Parameters") {
    return {
      status: 400,
      headers: { "Content-Type": "application/fhir+json", ...corsHeaders },
      jsonBody: createOperationOutcome(
        "error",
        "invalid",
        "Expected a FHIR Parameters resource as the request body"
      ),
    };
  }

  // Extract named parameters into a map
  const params = {};
  for (const param of inputParameters.parameter ?? []) {
    params[param.name] = param;
  }

  // Validate required parameters
  if (!params.expression?.valueString) {
    return {
      status: 400,
      headers: { "Content-Type": "application/fhir+json", ...corsHeaders },
      jsonBody: createOperationOutcome(
        "error",
        "required",
        "Missing required parameter: expression"
      ),
    };
  }

  if (!params.resource) {
    return {
      status: 400,
      headers: { "Content-Type": "application/fhir+json", ...corsHeaders },
      jsonBody: createOperationOutcome(
        "error",
        "required",
        "Missing required parameter: resource"
      ),
    };
  }

  const expression = params.expression.valueString;
  const contextExpr = params.context?.valueString ?? null;

  // Resolve the resource — may be inline or wrapped in a json-value extension
  let fhirData = params.resource.resource ?? null;
  if (!fhirData) {
    const extensions = params.resource.extension ?? [];
    const jsonExt = extensions.find(
      (e) =>
        e.url ===
          "http://fhir.forms-lab.com/StructureDefinition/json-value" ||
        e.url ===
          "http://fhir.forms-lab.com/StructureDefinition/xml-value"
    );
    if (jsonExt?.valueString) {
      try {
        fhirData = JSON.parse(jsonExt.valueString);
      } catch (_err) {
        return {
          status: 400,
          headers: { "Content-Type": "application/fhir+json", ...corsHeaders },
          jsonBody: createOperationOutcome(
            "error",
            "invalid",
            "Could not parse JSON from resource extension"
          ),
        };
      }
    }
  }

  if (!fhirData) {
    return {
      status: 400,
      headers: { "Content-Type": "application/fhir+json", ...corsHeaders },
      jsonBody: createOperationOutcome(
        "error",
        "required",
        "Could not extract a resource from the parameters"
      ),
    };
  }

  const resourceJson = JSON.stringify(fhirData);

  // Build the echo-back parameters part
  const parametersPart = {
    name: "parameters",
    part: [
      {
        name: "evaluator",
        valueString: `${EVALUATOR_NAME} (${schema})`,
      },
      { name: "expression", valueString: expression },
    ],
  };

  if (contextExpr) {
    parametersPart.part.push({ name: "context", valueString: contextExpr });
  }

  if (fhirData.resourceType) {
    parametersPart.part.push({ name: "resource", resource: fhirData });
  } else {
    parametersPart.part.push({
      name: "resource",
      extension: [
        {
          url: "http://fhir.forms-lab.com/StructureDefinition/json-value",
          valueString: resourceJson,
        },
      ],
    });
  }

  const outputParameters = {
    resourceType: "Parameters",
    parameter: [parametersPart],
  };

  // Evaluate the expression, optionally in the context of each context item
  try {
    if (contextExpr) {
      // Step 1 – evaluate the context expression to get a list of context items
      const contextItems = [];
      for (const node of engine.eval({
        expr: contextExpr,
        input: resourceJson,
        schema,
        adapter: "wasm",
      })) {
        contextItems.push(node.data);
      }

      // Step 2 – for each context item evaluate the main expression
      contextItems.forEach((item, idx) => {
        const contextPath = `${contextExpr}[${idx}]`;

        let contextJson;
        if (item !== null && typeof item === "object") {
          contextJson = JSON.stringify(item);
        } else {
          // Primitive context item – wrap in a minimal object so the engine
          // can still evaluate scalar-only expressions against it.
          contextJson = JSON.stringify({ value: item });
        }

        const resultParts = evaluateAndCollect(
          engine,
          expression,
          contextJson,
          schema
        );

        outputParameters.parameter.push({
          name: "result",
          valueString: contextPath,
          part: resultParts,
        });
      });
    } else {
      // No context – evaluate directly against the resource
      const resultParts = evaluateAndCollect(
        engine,
        expression,
        resourceJson,
        schema
      );
      outputParameters.parameter.push({
        name: "result",
        part: resultParts,
      });
    }
  } catch (err) {
    return {
      status: 400,
      headers: { "Content-Type": "application/fhir+json", ...corsHeaders },
      jsonBody: createOperationOutcome(
        "error",
        "invalid",
        `FHIRPath evaluation error: ${err.message}`
      ),
    };
  }

  return {
    status: 200,
    headers: { "Content-Type": "application/fhir+json", ...corsHeaders },
    jsonBody: outputParameters,
  };
}

// ──────────────────────────────────────────
//  Route registrations
// ──────────────────────────────────────────

app.http("fhirpath-r5", {
  methods: ["POST", "OPTIONS"],
  authLevel: "anonymous",
  route: "$fhirpath-r5",
  handler: (request, context) =>
    handleFhirPathRequest(request, context, "r5"),
});

app.http("fhirpath-r4", {
  methods: ["POST", "OPTIONS"],
  authLevel: "anonymous",
  route: "$fhirpath-r4",
  handler: (request, context) =>
    handleFhirPathRequest(request, context, "r4"),
});

app.http("health", {
  methods: ["GET", "OPTIONS"],
  authLevel: "anonymous",
  route: "health",
  handler: async (request, _context) => {
    const origin = request.headers.get("origin") || "";
    const cors = getCorsHeaders(origin);
    if (request.method === "OPTIONS") {
      return { status: 204, headers: cors };
    }
    await initEngines().catch(() => {});
    const available = Object.keys(engines);
    return {
      status: available.length > 0 ? 200 : 503,
      headers: { "Content-Type": "application/json", ...cors },
      jsonBody: {
        status: available.length > 0 ? "ok" : "unavailable",
        engine: EVALUATOR_NAME,
        schemas: available,
      },
    };
  },
});

app.http("config", {
  methods: ["GET", "OPTIONS"],
  authLevel: "anonymous",
  route: "config.json",
  handler: async (request, _context) => {
    const origin = request.headers.get("origin") || "";
    const cors = getCorsHeaders(origin);
    if (request.method === "OPTIONS") {
      return { status: 204, headers: cors };
    }
    const configPath = path.join(FUNCTION_ROOT, "config.json");
    const config = JSON.parse(fs.readFileSync(configPath, "utf-8"));

    // Derive base URL from the incoming request
    const proto = request.headers.get("x-forwarded-proto") || "https";
    const host = request.headers.get("x-forwarded-host") || request.headers.get("host") || "";
    const baseUrl = host ? `${proto}://${host}` : "";

    // Resolve relative paths in server URL entries
    for (const key of Object.keys(config)) {
      if (typeof config[key] === "string" && config[key].startsWith("/")) {
        config[key] = `${baseUrl}${config[key]}`;
      }
    }

    return {
      status: 200,
      headers: { "Content-Type": "application/json", ...cors },
      jsonBody: config,
    };
  },
});

app.http("info", {
  methods: ["GET", "OPTIONS"],
  authLevel: "anonymous",
  route: "/",
  handler: async (request, _context) => {
    const origin = request.headers.get("origin") || "";
    const cors = getCorsHeaders(origin);
    if (request.method === "OPTIONS") {
      return { status: 204, headers: cors };
    }
    await initEngines().catch(() => {});
    const available = Object.keys(engines);
    return {
      status: 200,
      headers: { "Content-Type": "application/json", ...cors },
      jsonBody: {
        engine: EVALUATOR_NAME,
        description:
          "FHIRPath evaluation API for the fhirpath-lab, powered by the fhirpath.zig WASM engine",
        schemas: available,
        endpoints: [
          {
            method: "POST",
            path: "/$fhirpath-r5",
            description:
              "Evaluate a FHIRPath expression using FHIR R5 schema awareness",
          },
          {
            method: "POST",
            path: "/$fhirpath-r4",
            description:
              "Evaluate a FHIRPath expression using FHIR R4 schema awareness",
          },
          {
            method: "GET",
            path: "/health",
            description: "Health check",
          },
        ],
      },
    };
  },
});
