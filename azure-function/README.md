# fhirpath.zig Azure Function App

An [Azure Function App](https://learn.microsoft.com/en-us/azure/azure-functions/) in Node.js that exposes the [fhirpath-lab server API](https://github.com/brianpos/fhirpath-lab/blob/master/server-api.md) backed by the **fhirpath.zig** WebAssembly engine.

This lets you run fhirpath.zig as an external API service and connect it to the [fhirpath-lab](https://fhirpath-lab.com) using [custom configuration](https://github.com/brianpos/fhirpath-lab/blob/develop/docs/custom-configuration.md).

## Endpoints

| Method | Path | Description |
|--------|------|-------------|
| `POST` | `/$fhirpath-r5` | Evaluate a FHIRPath expression with FHIR R5 schema awareness |
| `POST` | `/$fhirpath-r4` | Evaluate a FHIRPath expression with FHIR R4 schema awareness |
| `GET`  | `/health` | Health check – lists available schemas |
| `GET`  | `/` | API info |

Both `POST` endpoints accept a [FHIR Parameters](https://www.hl7.org/fhir/parameters.html) resource and return a FHIR Parameters resource, per the [fhirpath-lab server API spec](https://github.com/brianpos/fhirpath-lab/blob/master/server-api.md).

CORS is enabled for:
- `https://fhirpath-lab.com`
- `https://dev.fhirpath-lab.com`
- `http://localhost:3000`

## Prerequisites

- [Zig](https://ziglang.org/download/) `0.15.2`
- [Node.js](https://nodejs.org/) `>=20`
- [Azure Functions Core Tools v4](https://learn.microsoft.com/en-us/azure/azure-functions/functions-run-local) (for local development)

## Setup

### 1 — Build the WASM engine and model files

From the **repository root**:

```bash
# Download FHIR StructureDefinitions
./scripts/fetch_models.sh

# Build the WASM binary
zig build wasm -Doptimize=ReleaseSmall

# Build compact model blobs (R4 + R5)
./scripts/build_models.sh
```

### 2 — Assemble the function app

```bash
./scripts/build_azure_function.sh
```

This copies `fhirpath.js`, `fhirpath.wasm`, `model-r5.bin`, and (optionally) `model-r4.bin` into the `azure-function/` directory and runs `npm install`.

### 3 — Run locally

```bash
cd azure-function
func start
```

The function will be available at `http://localhost:7071`.

## Example request

```bash
curl -X POST http://localhost:7071/\$fhirpath-r5 \
  -H "Content-Type: application/fhir+json" \
  -d '{
    "resourceType": "Parameters",
    "parameter": [
      {
        "name": "expression",
        "valueString": "name.given"
      },
      {
        "name": "resource",
        "resource": {
          "resourceType": "Patient",
          "name": [{ "given": ["Alice", "Bob"], "family": "Smith" }]
        }
      }
    ]
  }'
```

## Connecting to the fhirpath-lab

See the [custom configuration docs](https://github.com/brianpos/fhirpath-lab/blob/develop/docs/custom-configuration.md) to point the fhirpath-lab at your deployed function URL.

Set the service URL to your function's base URL (e.g. `https://<your-function>.azurewebsites.net` or `http://localhost:7071` for local testing).

## Deploying to Azure

```bash
# From the azure-function/ directory (after running build_azure_function.sh)
func azure functionapp publish <YOUR_FUNCTION_APP_NAME>
```

Ensure the Function App is configured for Node.js 20+ in the Azure portal.

## CI Artifact

The GitHub Actions CI workflow builds a ready-to-deploy `azure-function-dist` artifact that includes all necessary files. Download it from the workflow run and run `npm install && func start` to use it.
