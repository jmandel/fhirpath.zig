package dev.exe;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.fhirpath.IFhirPath;
import ca.uhn.fhir.parser.IParser;
import com.google.gson.*;
import org.hl7.fhir.instance.model.api.IBase;
import org.hl7.fhir.r5.model.*;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class FhirPathRunner {
    private static final FhirContext ctx = FhirContext.forR5();
    private static final IFhirPath fhirPath = ctx.newFhirPath();
    private static final IParser jsonParser = ctx.newJsonParser();
    private static final Gson gson = new GsonBuilder().serializeNulls().create();

    public static void main(String[] args) {
        if (args.length > 0 && args[0].equals("--batch")) {
            batchMode();
        } else {
            singleMode(args);
        }
    }

    private static void batchMode() {
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            String input = reader.lines().collect(Collectors.joining("\n"));
            
            JsonArray tests = JsonParser.parseString(input).getAsJsonArray();
            JsonArray results = new JsonArray();
            
            for (JsonElement testEl : tests) {
                JsonObject test = testEl.getAsJsonObject();
                String expr = test.get("expr").getAsString();
                JsonElement inputEl = test.get("input");
                
                try {
                    List<TypedValue> evalResult = evaluateExpression(expr, inputEl);
                    JsonObject result = new JsonObject();
                    JsonArray resultsArray = new JsonArray();
                    for (TypedValue tv : evalResult) {
                        resultsArray.add(tv.toJson());
                    }
                    result.add("results", resultsArray);
                    results.add(result);
                } catch (Exception e) {
                    JsonObject result = new JsonObject();
                    result.addProperty("error", e.getMessage());
                    results.add(result);
                }
            }
            
            System.out.println(gson.toJson(results));
        } catch (Exception e) {
            System.err.println("Batch error: " + e.getMessage());
            System.exit(1);
        }
    }

    private static void singleMode(String[] args) {
        if (args.length == 0) {
            System.out.println("Usage: FhirPathRunner <expression> [json]");
            System.out.println("       FhirPathRunner --batch < tests.json");
            System.exit(1);
        }

        String expression = args[0];
        String jsonInput = args.length > 1 ? args[1] : "{}";

        try {
            JsonElement inputEl = JsonParser.parseString(jsonInput);
            List<TypedValue> result = evaluateExpression(expression, inputEl);
            
            JsonArray output = new JsonArray();
            for (TypedValue tv : result) {
                output.add(tv.toJson());
            }
            System.out.println(gson.toJson(output));
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(1);
        }
    }

    private static List<TypedValue> evaluateExpression(String expression, JsonElement input) {
        IBase resource = null;
        
        if (input != null && input.isJsonObject()) {
            JsonObject obj = input.getAsJsonObject();
            if (obj.has("resourceType")) {
                // Parse as FHIR resource
                resource = jsonParser.parseResource(gson.toJson(obj));
            }
        }
        
        if (resource == null) {
            // Use a dummy Patient for context-free expressions
            resource = new Patient();
        }
        
        List<IBase> evalResult = fhirPath.evaluate(resource, expression, IBase.class);
        
        List<TypedValue> results = new ArrayList<>();
        for (IBase item : evalResult) {
            results.add(getTypeAndValue(item));
        }
        return results;
    }

    private static TypedValue getTypeAndValue(IBase item) {
        if (item == null) {
            return new TypedValue("null", JsonNull.INSTANCE);
        }
        
        if (item instanceof BooleanType) {
            return new TypedValue("boolean", new JsonPrimitive(((BooleanType) item).getValue()));
        }
        if (item instanceof IntegerType) {
            return new TypedValue("integer", new JsonPrimitive(((IntegerType) item).getValue()));
        }
        if (item instanceof Integer64Type) {
            return new TypedValue("integer", new JsonPrimitive(((Integer64Type) item).getValue()));
        }
        if (item instanceof DecimalType) {
            BigDecimal val = ((DecimalType) item).getValue();
            return new TypedValue("decimal", new JsonPrimitive(val));
        }
        if (item instanceof StringType) {
            return new TypedValue("string", new JsonPrimitive(((StringType) item).getValue()));
        }
        if (item instanceof DateType) {
            return new TypedValue("date", new JsonPrimitive(((DateType) item).getValueAsString()));
        }
        if (item instanceof DateTimeType) {
            return new TypedValue("dateTime", new JsonPrimitive(((DateTimeType) item).getValueAsString()));
        }
        if (item instanceof InstantType) {
            return new TypedValue("dateTime", new JsonPrimitive(((InstantType) item).getValueAsString()));
        }
        if (item instanceof TimeType) {
            return new TypedValue("time", new JsonPrimitive(((TimeType) item).getValue()));
        }
        if (item instanceof Quantity) {
            Quantity q = (Quantity) item;
            JsonObject qObj = new JsonObject();
            qObj.addProperty("value", q.getValue());
            qObj.addProperty("unit", q.getUnit() != null ? q.getUnit() : "");
            return new TypedValue("Quantity", qObj);
        }
        if (item instanceof UriType) {
            return new TypedValue("string", new JsonPrimitive(((UriType) item).getValue()));
        }
        if (item instanceof CodeType) {
            return new TypedValue("string", new JsonPrimitive(((CodeType) item).getValue()));
        }
        if (item instanceof IdType) {
            return new TypedValue("string", new JsonPrimitive(((IdType) item).getValue()));
        }
        
        // For complex types, return the type name and try to serialize
        String typeName = item.fhirType();
        try {
            if (item instanceof org.hl7.fhir.r5.model.Resource) {
                String json = jsonParser.encodeResourceToString((org.hl7.fhir.r5.model.Resource) item);
                return new TypedValue(typeName, JsonParser.parseString(json));
            }
        } catch (Exception e) {
            // Fall through
        }
        
        return new TypedValue(typeName, new JsonPrimitive(item.toString()));
    }

    private static class TypedValue {
        String type;
        JsonElement value;

        TypedValue(String type, JsonElement value) {
            this.type = type;
            this.value = value;
        }

        JsonObject toJson() {
            JsonObject obj = new JsonObject();
            obj.addProperty("type", type);
            obj.add("value", value);
            return obj;
        }
    }
}
