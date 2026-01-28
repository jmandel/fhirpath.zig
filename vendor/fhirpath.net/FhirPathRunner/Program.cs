using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Nodes;
using Hl7.Fhir.ElementModel;
using Hl7.FhirPath;
using Hl7.Fhir.FhirPath;
using Hl7.Fhir.Model;
using Hl7.Fhir.Serialization;

class Program
{
    static void Main(string[] args)
    {
        if (args.Length > 0 && args[0] == "--batch")
        {
            BatchMode();
        }
        else
        {
            SingleMode(args);
        }
    }

    static void BatchMode()
    {
        var input = Console.In.ReadToEnd();
        var tests = JsonSerializer.Deserialize<List<TestCase>>(input);
        var results = new List<object>();

        foreach (var test in tests!)
        {
            try
            {
                var result = EvaluateExpression(test.expr, test.input);
                results.Add(new { results = result });
            }
            catch (Exception e)
            {
                results.Add(new { error = e.Message });
            }
        }

        Console.WriteLine(JsonSerializer.Serialize(results));
    }

    static void SingleMode(string[] args)
    {
        if (args.Length == 0)
        {
            Console.WriteLine("Usage: FhirPathRunner <expression> [json]");
            Console.WriteLine("       FhirPathRunner --batch < tests.json");
            Environment.Exit(1);
        }

        var expression = args[0];
        var jsonInput = args.Length > 1 ? args[1] : "{}";

        try
        {
            JsonNode? inputNode = JsonNode.Parse(jsonInput);
            var result = EvaluateExpression(expression, inputNode);
            Console.WriteLine(JsonSerializer.Serialize(result, new JsonSerializerOptions { WriteIndented = true }));
        }
        catch (Exception e)
        {
            Console.Error.WriteLine($"Error: {e.Message}");
            Environment.Exit(1);
        }
    }

    static List<TypedValue> EvaluateExpression(string expression, JsonNode? input)
    {
        Base? resource = null;
        
        if (input != null && !(input is JsonObject obj && obj.Count == 0))
        {
            var jsonStr = input.ToJsonString();
            try
            {
                var deserializer = new FhirJsonDeserializer();
                resource = deserializer.Deserialize<Resource>(jsonStr);
            }
            catch
            {
                // Not a FHIR resource - try to evaluate on empty context
                resource = null;
            }
        }

        IEnumerable<Base?> evalResult;
        if (resource != null)
        {
            // Use POCO-based evaluation
            evalResult = resource.Select(expression, new FhirEvaluationContext());
        }
        else
        {
            // Create a minimal resource for context-free expressions
            var dummyResource = new Patient();
            evalResult = dummyResource.Select(expression, new FhirEvaluationContext());
        }

        var results = new List<TypedValue>();
        foreach (var item in evalResult)
        {
            results.Add(GetTypeAndValue(item));
        }
        return results;
    }

    static TypedValue GetTypeAndValue(object item)
    {
        if (item == null)
            return new TypedValue("null", null);
        
        if (item is bool b)
            return new TypedValue("boolean", b);
        
        if (item is int i)
            return new TypedValue("integer", i);
        
        if (item is long l)
            return new TypedValue("integer", l);
        
        if (item is decimal d)
            return new TypedValue("decimal", d);
        
        if (item is double dbl)
            return new TypedValue("decimal", dbl);
        
        if (item is string s)
            return new TypedValue("string", s);
        
        if (item is Hl7.Fhir.ElementModel.Types.Date dt)
            return new TypedValue("date", dt.ToString());
        
        if (item is Hl7.Fhir.ElementModel.Types.DateTime dtm)
            return new TypedValue("dateTime", dtm.ToString());
        
        if (item is Hl7.Fhir.ElementModel.Types.Time tm)
            return new TypedValue("time", tm.ToString());
        
        if (item is Hl7.Fhir.ElementModel.Types.Quantity q)
            return new TypedValue("Quantity", new QuantityValue { value = q.Value, unit = q.Unit ?? "" });
        
        if (item is Quantity fq)
            return new TypedValue("Quantity", new QuantityValue { value = fq.Value ?? 0, unit = fq.Unit ?? "" });
        
        // For FHIR model types
        if (item is Base fhirBase)
        {
            var typeName = fhirBase.TypeName;
            // Try to get a simple value representation
            if (fhirBase is PrimitiveType pt && pt.ObjectValue != null)
            {
                return GetTypeAndValue(pt.ObjectValue);
            }
            // Return as complex object
            return new TypedValue(typeName, SerializeBase(fhirBase));
        }
        
        return new TypedValue(item.GetType().Name, item?.ToString());
    }

    static object? SerializeBase(Base fhirBase)
    {
        try
        {
            var serializer = new FhirJsonSerializer();
            var json = serializer.SerializeToString(fhirBase);
            return JsonSerializer.Deserialize<JsonElement>(json);
        }
        catch
        {
            return fhirBase.ToString();
        }
    }
}

class TestCase
{
    public string expr { get; set; } = "";
    public JsonNode? input { get; set; }
}

class TypedValue
{
    public string type { get; set; }
    public object? value { get; set; }

    public TypedValue(string type, object? value)
    {
        this.type = type;
        this.value = value;
    }
}

class QuantityValue
{
    public decimal value { get; set; }
    public string unit { get; set; } = "";
}
