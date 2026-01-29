




This is the Continuous Integration Build of FHIR (will be incorrect/inconsistent at times).  
See the [Directory of published versions](http://hl7.org/fhir/directory.html)

## 2.1.9 FHIRPath

|  |  |
|----|----|
| Responsible Owner: FHIR Infrastructure Work Group | [Standards Status](versions.html#std-process): [Normative](versions.html#std-process) |

The FHIR Specification uses [FHIRPath (release 2)](http://hl7.org/fhirpath/r2) for path-based navigation and extraction. FHIRPath is a separate specification published at [http://hl7.org/fhirpath](http://hl7.org/fhirpath/r2) in order to support wider re-use across multiple specifications.

FHIRPath is used in several places in the FHIR and related specifications:

- [invariants in ElementDefinition](elementdefinition-definitions.html#ElementDefinition.constraint.expression) - used to apply co-occurrence and other rules to the contents (e.g., value.empty() or code!=component.code)
- [slicing discriminator](elementdefinition-definitions.html#ElementDefinition.slicing.discriminator.path) - used to indicate what element(s) define uniqueness (e.g., Observation.category)
- [search parameter paths](searchparameter-definitions.html#SearchParameter.expression) - used to define what contents the parameter refers to (e.g., Observation.dataAbsentReason)
- [error message locations in OperationOutcome](operationoutcome-definitions.html#OperationOutcome.issue.expression)
- [FHIRPath-based Patch](fhirpatch.html)
- [Invariants in the TestScript resource](https://build.fhir.org/ig/hl7/fhir-testing-ig/index.html/StructureDefinitions-TestScript-definitions.html#TestScript.setup.action.assert.expression)

FHIRPath is used and extended in many other contexts beyond those listed above. For additional examples and information about other usage patterns and extensions, refer to the [FHIRPath Usage and Extensions page](https://confluence.hl7.org/spaces/FHIRI/pages/391650086/FHIRPath+Usage+and+Extensions) on HL7's confluence.


<img src="assets/images/dragon.png" title="Here Be Dragons!" style="float:left; mix-blend-mode: multiply; margin-right: 10px;" width="150" height="150" />

#### 2.1.9.0.1 FHIR Inter-version issues

FHIR inter-version compatibility rules allow for non-repeating elements to be made repeating in future FHIR versions. For this reason, FHIRPath expressions SHOULD be written presuming all elements will repeat, even if the current version of the model has a maximum cardinality of '1'.


 



### 2.1.9.1 Using FHIRPath with Resources

In FHIRPath, like XPath, operations are expressed in terms of the logical content of hierarchical data models, and support traversal, selection and filtering of data.

FHIRPath uses a tree model that abstracts away the actual underlying data model of the data being queried. For FHIR, this means that the contents of the resources and data types as described in the Logical views (or the UML diagrams) are used as the model, rather than the JSON and XML formats, so specific xml or json features are not visible to the FHIRPath language (such as comments and the split representation of primitives).

More specifically:

- A FHIRPath may optionally start with a full resource name
- Elements of datatypes and resources are used as the name of the nodes which can be navigated over, except for choice elements (ending with '\[x\]'), see below.
- The `contained` element node does not have the name of the Resource as its first and only child (instead it directly contains the contained resource’s children)
- There is no difference between an attribute and an element
- Repeating elements turn into multiple nodes with the same name



#### 2.1.9.1.1 Polymorphism in FHIR

For [choice elements](formats.html#choice), where elements can be one of multiple types, e.g., `Patient.deceased[x]`. In actual instances these will be present as either `Patient.deceasedBoolean` or `Patient.deceasedDateTime`. In FHIRPath, choice elements are labeled according to the name without the '\[x\]' suffix, and children can be explicitly treated as a specific type using the `ofType` function:

``` fhirpath
(Observation.value.ofType(Quantity)).unit
```

FHIRPath statements can start with a full resource name:

    Patient.name.given

The name can also include super types such as DomainResource:

    DomainResource.contained(id = 23).exists()

These statements apply to any resource that specializes [DomainResource](domainresource.html).



#### 2.1.9.1.2 Using FHIR types in expressions

The namespace for the types defined in FHIR (primitive datatypes, datatypes, resources) is FHIR. So, for example:

    Patient.is(FHIR.Patient)

The first element - the type name - is not namespaced, but the parameter to the is() operation is.

Understanding the primitive types is critical: FHIR.string is a different type to System.String. The FHIR.string type specializes FHIR.Element, and has the properties id, extension, and also the implicit value property that is actually of type of System.String.

The evaluation engine will automatically convert the value of FHIR types representing primitives to FHIRPath types when they are used in expressions according to the following mapping:

| FHIR primitive type | FHIRPath type |
|----|----|
| FHIR.boolean | System.Boolean |
| FHIR.string, FHIR.uri, FHIR.code, FHIR.oid, FHIR.id, FHIR.uuid, FHIR.markdown, FHIR.base64Binary | System.String |
| FHIR.integer, FHIR.unsignedInt, FHIR.positiveInt | System.Integer |
| FHIR.integer64 | System.Long |
| FHIR.decimal | System.Decimal |
| FHIR.date, FHIR.dateTime, FHIR.instant | System.DateTime |
| FHIR.time | System.Time |
| FHIR.Quantity | System.Quantity (see below) |

Since FHIR primitives may contain extensions, the following expressions are *not* mutually exclusive:

``` fhirpath
Patient.name.given = 'Ewout'                          // value of Patient.name.given as a string
Patient.name.given.extension.first().value = true     // extension of the primitive value
!Patient.active.hasValue() and !Patient.active.empty() // can both be true, if there are extensions but no value for Patient.active
```

The automatic conversion means that in most respects, a FHIR primitive can generally be treated as if it was the equivalent FHIRPath system type. The primary exception is the is() operation, where the difference is explicit:

``` fhirpath
Patient.name.given.is(FHIR.string)
Patient.name.given.is(System.string).not()
Patient.name.given.getValue().is(System.string)
```

This also applies to e.g. Resource.id:

``` fhirpath
Patient.id is System.String
(Patient.id is FHIR.id).not
```

As shown, all FHIR primitives have the operation `getValue()` defined (see below) for the few edge cases where the automatic conversion isn't appropriate. Note that ofType() does not have such restrictions - both of the following are valid:

``` fhirpath
Patient.name.given.ofType(FHIR.string)
Patient.name.given.ofType(System.string)
```



#### 2.1.9.1.3 Use of FHIR Quantity

The Mapping from FHIR Quantity to FHIRPath System.Quantity can only be applied if the FHIR Quantity has a UCUM code - i.e., a system of `http://unitsofmeasure.org`, and a code is present.

As part of the mapping, time-valued UCUM units are mapped to the [calendar duration units](http://hl7.org/fhirpath/R2/index.html#time-valued-quantities) defined in FHIRPath, according to the following map:

|     |        |
|-----|--------|
| a   | year   |
| mo  | month  |
| d   | day    |
| h   | hour   |
| min | minute |
| s   | second |


<img src="assets/images/dragon.png" title="Here Be Dragons!" style="float:left; mix-blend-mode: multiply; margin-right: 10px;" width="150" height="150" />

i.e., The FHIR Quantity 1 'a' would be implicitly converted to the FHIRPath System.Quantity 1 'year'. Note that there's a subtle difference between the UCUM definitions for `a` and `mo`, which are definition durations of `365.25` and `30` days respectively, while `year` and `month` are calendar based durations, and their length of time varies. See [Time-valued Quantities](http://hl7.org/fhirpath/N1/#time-valued-quantities) for further discussion. Implementers should be aware of these subtle differences, but in general, this approach matches what users expect most closely.


 



#### 2.1.9.1.4 FHIR Specific Variables

FHIR defines two specific variables that are always in scope when FHIRPath is used in any of the contexts above:

    %resource     // the resource that contains the original node that is in %context
    %rootResource // the container resource for the resource identified by %resource
    %profile      // the canonical URL of a profile that contains the invariant's fhirpath expression (only defined in profile invariants)
                  // Note: not all fhirpath engines support this profile variable, or the slice() function that usually uses it.

The resource is very often the context, such that %resource = %context. When a DomainResource contains another resource, and that contained resource is the focus (%resource) then %rootResource refers to the container resource. Note that in most cases, the resource is not contained by another resource, and then %rootResource is the same as %resource.

In addition to the general purpose variables above, additional variables and/or guidance about variable use are defined when working with specific resources. Refer to the following for additional FHIRPath guidance defined for:

- [SubscriptionTopic](subscriptiontopic.html#resource-trigger-update)
- [QuestionnaireResponse](questionnaireresponse.html#fhirpath)



#### 2.1.9.1.5 Additional functions

FHIR adds (compatible) functionality to the set of common FHIRPath functions. Some of these functions are candidates for elevation to the base version of FHIRPath when the next version is released.

In addition to the general purpose functions below, additional functions function use are defined when working with specific resources. Refer to the following for additional FHIRPath guidance defined for:

- [QuestionnaireResponse](questionnaireresponse.html#fhirpath)



##### 2.1.9.1.5.1 **extension(url : string) : collection**

Will filter the input collection for items named "extension" with the given `url`. This is a syntactical shortcut for `.extension.where(url = string)`, but is simpler to write. If the input collection is empty or the url is empty, the return value will be an empty collection.

``` fhirpath
Patient.extension('http://hl7.org/fhir/StructureDefinition/patient-mothersMaidenName').value
```

------------------------------------------------------------------------



##### 2.1.9.1.5.2 **hasValue() : Boolean**

If the input collection contains a single value which is a FHIR primitive, then will return true if the single value is a primitive value (e.g., as opposed to not having a value and just having extensions) and false if not. If the input collection isn't a single FHIR primitive, the return value is empty.

> **Note to implementers**: The FHIR conceptual model talks about "primitives" as subclasses of the type Element that also have id and extensions. What this actually means is that a FHIR primitive is not a primitive in an implementation language. The introduction (section 2 above) describes the navigation tree as if the FHIR model applies - primitives are both primitives and elements with children.
>
> In FHIRPath, this means that FHIR primitives have a `value` child, but, as described above, they are automatically cast to FHIRPath primitives when comparisons are made, and that the primitive value will be included in the set returned by `children()` or `descendants()`.

``` fhirpath
Patient.active.hasValue()
```

------------------------------------------------------------------------



##### 2.1.9.1.5.3 **toString() : string**

If the input collection contains a single item, this function will return a single String if the type is a primitive, and it has a value (e.g. if `hasValue() = true`), and the primitive type has a System type for which [toString() is defined](https://hl7.org/fhirpath/N1/#tostring-string) . Otherwise the result is an empty collection.

``` fhirpath
Patient.birthDate.toString()
```

------------------------------------------------------------------------



##### 2.1.9.1.5.4 **getValue() : System.\[type\]**

Return the underlying system value for the FHIR primitive if the input collection contains a single value which is a FHIR primitive, and it has a primitive value (see discussion for hasValue()). Otherwise, the return value is empty.

``` fhirpath
Patient.birthDate.getValue() // returns just the primitive, does not include extensions (such as the birthTime extension)
```

------------------------------------------------------------------------



##### 2.1.9.1.5.5 **resolve() : collection**

For each item in the collection, if it is a string that is a [uri](datatypes.html#uri) (or [canonical](datatypes.html#canonical) or [url](datatypes.html#url)), locate the target of the reference, and add it to the resulting collection. If the item does not resolve to a resource, the item is ignored and nothing is added to the output collection.

The items in the collection may also represent a Reference, in which case the `Reference.reference` is resolved. If the input is empty, the return value will be an empty collection.

``` fhirpath
Patient.managingOrganization.resolve().name
Observation.subject.where(resolve() is Patient)
```

------------------------------------------------------------------------



##### 2.1.9.1.5.6 **ofType(type : type specifier) : collection**

Returns a collection that contains all items in the input collection that are of the given `type` or a subclass thereof. This is as defined as in the base FHIRPath specification, but implementers should be aware that in FHIR, only concrete core types are allowed as an argument. All primitives are considered to be independent types (so `markdown` is **not** a subclass of `string`). Profiled types are not allowed, so to select `SimpleQuantity` one would pass `Quantity` as an argument.

``` fhirpath
Observation.value.ofType(canonical)
Observation.value.ofType(dateTime) | Observation.value.ofType(Period).start // selects either dateTime or Period starts (both of type dateTime)
```

------------------------------------------------------------------------



##### 2.1.9.1.5.7 **elementDefinition() : collection**

Returns the FHIR element definition information for each element in the input collection. If the input collection is empty, the return value will be an empty collection.

------------------------------------------------------------------------



##### 2.1.9.1.5.8 **slice(structure : string, name : string) : collection**

Returns the input collection filtered to items that conform to the specified profile slice.

The `structure` argument is the (possibly versioned) canonical uri of the structure definition that contains the specified slice.  
The `name` argument is either the `ElementDefinition.sliceName` for the element slice (if the sliceName is unique in the snapshot), or the `ElementDefinition.id` of the element slice.

The `structure` argument can also be the special variable `%profile` which refers to the profile in which the ElementDefinition.constraint containing the FHIRPath is defined. (This variable can only be used for FHIRPath appearing in constraints)

If the `structure` cannot be resolved, or the `name` of the slice is not present in the profile, or those parameters are empty, the return value will be an empty collection.

If the slice does not match any element in the input collection, or if the input collection is empty, the return value will be an empty collection.

For example, this invariant could appear in a Patient profile:

``` fhirpath
// ensure that a patient has at least one home phone and one home email (based on the slicing defined in the profile)
Patient.telecom.slice(%profile, 'homephone').exists() and Patient.telecom.slice(%profile, 'homeemail').exists()
```

> NOTE: not all FHIRPath engines are in a position to implement `slice()` as implementation requires implementation of a FHIR validation engine, which is not a small undertaking.

------------------------------------------------------------------------



##### 2.1.9.1.5.9 **checkModifiers(modifier : string) : collection**

For each element in the input collection, verifies that there are no modifying extensions defined other than the ones given by the `modifier` argument (comma-separated string). If the check passes, the input collection is returned. Otherwise, an error is thrown, including if modifier is empty.

------------------------------------------------------------------------



##### 2.1.9.1.5.10 **conformsTo(structure : string) : Boolean**

Returns `true` if the single input element conforms to the profile specified by the `structure` argument, and false otherwise. If the input is not a single item, the structure is empty, or the structure cannot be resolved to a valid profile, the return value is empty.

``` fhirpath
Patient.conformsTo('http://hl7.org/fhir/StructureDefinition/Patient')
```

------------------------------------------------------------------------



##### 2.1.9.1.5.11 **memberOf(valueset : string) : Boolean**

When invoked on a single code-valued element, returns true if the code is a member of the given `valueset`. When invoked on a single concept-valued element, returns true if any code in the concept is a member of the given valueset. When invoked on a single string, returns true if the string is equal to a code in the valueset, so long as the valueset only contains one codesystem. If the valueset in this case contains more than one codesystem, the return value is empty.

If the valueset cannot be resolved as a uri to a value set, or the input is empty or has more than one value, the return value is empty.

Note that implementations are encouraged to make use of a terminology service to provide this functionality.

For example:

``` fhirpath
Observation.component.where(code.memberOf('http://hl7.org/fhir/ValueSet/observation-vitalsignresult'))
```

This expression returns components that have a code that is a member of the observation-vitalsignresult valueset.

------------------------------------------------------------------------



##### 2.1.9.1.5.12 **subsumes(code : Coding \| CodeableConcept) : Boolean**

When invoked on a Coding-valued element and the given `code` is Coding-valued, returns true if the source code is equivalent to the given code, or if the source code subsumes the given code (i.e., the source code is an ancestor of the given code in a subsumption hierarchy), and false otherwise.

If the Codings are from different code systems, the relationships between the code systems must be well-defined or the return value is empty.

When the source or given elements are CodeableConcepts, returns true if any Coding in the source or given elements is equivalent to or subsumes the given code.

If either the input or the code parameter are not single value collections, the return value is empty.

Note that implementations are encouraged to make use of a terminology service to provide this functionality.

``` fhirpath
%terminologies.subsumes(Procedure.category, Procedure.code) = 'subsumes'
```

------------------------------------------------------------------------



##### 2.1.9.1.5.13 **subsumedBy(code: Coding \| CodeableConcept) : Boolean**

When invoked on a Coding-valued element and the given `code` is Coding-valued, returns true if the source code is equivalent to the given code, or if the source code is subsumed by the given code (i.e., the given code is an ancestor of the source code in a subsumption hierarchy), and false otherwise.

If the Codings are from different code systems, the relationships between the code systems must be well-defined or a run-time error is thrown.

When the source or given elements are CodeableConcepts, returns true if any Coding in the source or given elements is equivalent to or subsumed by the given code.

If either the input or the code parameter are not single value collections, the return value is empty.

Note that implementations are encouraged to make use of a terminology service to provide this functionality.

------------------------------------------------------------------------



##### 2.1.9.1.5.14 **htmlChecks : Boolean**

When invoked on a single [xhtml](narrative.html#xhtml) element returns true if the [rules around HTML usage](narrative.html#rules) are met, and false if they are not. The return value is empty on any other kind of element, or a collection of xhtml elements.

------------------------------------------------------------------------



##### 2.1.9.1.5.15 **comparable(quantity) : boolean**

This function returns true if the engine executing the FHIRPath statement can compare the singleton Quantity with the singleton other Quantity and determine their relationship to each other. Comparable means that both have values and that the code and system for the units are the same (irrespective of system) or both have code + system, system is recognized by the FHIRPath implementation and the codes are comparable within that code system. e.g., days and hours or inches and cm.

This function is intended to be added to the core FHIRPath specification in a future version.

------------------------------------------------------------------------



##### 2.1.9.1.5.16 **weight() : decimal**

This functions returns the ordinal value for an element. This may be based on an [itemWeight extension](https://build.fhir.org/ig/HL7/fhir-extensions/StructureDefinition-itemWeight.html) defined on an element such as QuestionnaireResponse answer, or it may be based on the [weight property](codesystem.html#defined-props) defined on a code. In some cases, this may require looking across resources. For example, QuestionnaireResponse.item.where(linkId='123').answer.first().weight() may need to find the corresponding item in the Questionnaire and then look up the coding specified in 'answer' with the corresponding answerOption in Questionnaire.item with the same linkId. If no weight is defined for the context element, the return value is empty. If the FHIRPath engine is unable to resolve the corresponding value set, code system or questionnaire options, it SHOULD cause the expression to fail. This function will typically be used to support scoring of Questionnaires, but it can be used with observations or potentially other elements as well. 

#### 2.1.9.1.6 Changes to operators

**~ (Equivalence)** 

Equivalence works in exactly the same manner, but with the addition that for complex types, equality requires all child properties to be equal, **except for "id" elements**.

In addition, for Coding values, equivalence is defined based on the code and system elements only. The version, display, and userSelected elements are ignored for the purposes of determining Coding equivalence.

For CodeableConcept values, equivalence is defined as a non-empty intersection of Coding elements, using equivalence. In other words, two CodeableConcepts are considered equivalent if any Coding in one is equivalent to any Coding in the other.



#### 2.1.9.1.7 Environment variables

The FHIR specification adds support for additional environment variables:

The following environmental values are set for all contexts:

``` fhirpath
%sct        // (string) url for snomed ct
%loinc      // (string) url for loinc
%`vs-[name]` // (string) full url for the provided HL7 value set with id [name]
%`ext-[name]` // (string) full url for the provided HL7 extension with id [name]
%resource   // The original resource current context is part of. When evaluating a datatype, this would be the resource the element is part of. Do not go past a root resource into a bundle, if it is contained in a bundle.

// Note that the names of the `vs-` and `ext-` constants are quoted (just like paths) to allow "-" in the name.
```

For example:

``` fhirpath
Observation.component.where(code.memberOf(%`vs-observation-vitalsignresult`))
```

This expression returns components that have a code that is a member of the observation-vitalsignresult valueset.

> **Implementation Note:** Implementation Guides are allowed to define their own externals, and implementers should provide some appropriate configuration framework to allow these constants to be provided to the evaluation engine at run-time. E.g.:
>
> ``` fhirpath
> %`us-zip` = '[0-9]{5}(-[0-9]{4}){0,1}'
> ```

Authors of Implementation Guides should be aware that adding specific environment variables restricts the use of the FHIRPath to their particular context.

Note that these tokens are not restricted to simple types, and they may have fixed values that are not known before evaluation at run-time, though there is no way to define these kinds of values in implementation guides.



### 2.1.9.2 Restricted Subset ("Simple")

This page documents a restricted subset of the [FHIRPath language](http://hl7.org/fhirpath) that is used in a few contexts in this specification. When the restricted FHIRPath language subset is in use, the following rules apply:

- All statements SHALL start with the name of the context element (e.g., on a Patient resource, Patient.contact.name.), or SHALL be simply "\$this" to refer to the element that has focus
- Operators SHALL NOT be used
- Only the following functions may be used:
  - .resolve()
  - .extension("url")
  - .ofType(type)

  All other functions SHALL NOT be used

These rules exist to keep processing the path simple to support use of the path by processors that are not backed by a full FHIRPath implementation.

The following locations use this restricted FHIRPath language:

- [ElementDefinition.slicing.discriminator.path](elementdefinition-definitions.html#ElementDefinition.slicing.discriminator.path)
- [DataRequirement.dateFilter.path](metadatatypes-definitions.html#DataRequirement.dateFilter.path)
- [OperationOutcome.issue.expression](operationoutcome-definitions.html#OperationOutcome.issue.expression)



### 2.1.9.3 Type Factory

The variable %factory is a reference to a class factory that provides the following type methods. Note that a future version of FHIRPath may provide a factory framework directly, in which case this factory API may be withdrawn or deprecated.

This API provides specific methods for constructing common types, and some general methods for constructing any type.

For the specific type constructors, all the parameters are optional. Note that since all variables / outputs in FHIRPath are collections, all the parameters are inherently collections, but when the underlying element referred to is a singleton element, the collection cannot contain more than one item. Use the value `{}` if there is no value to provide.

------------------------------------------------------------------------



#### 2.1.9.3.1 %factory.{primitive}(value, extensions) : {primitive}

Create an instance of the type with the value and possibly one or more extensions. e.g., `%factory.code('final')`.

Parameters:

- **value**: a primitive type (string, or will be converted to a string if necessary) that contains the value for the primitive type.
- **extensions**: a collection of extensions for the primitive type

**Return Value:** the primitive type, or an error.

------------------------------------------------------------------------



#### 2.1.9.3.2 %factory.Extension(url, value) : Extension

Creates an extension with the given url and value: `%factory.extension('http://hl7.org/fhir/StructureDefinition/artifact-copyrightLabel', 'CC0-1.0')`.

Parameters:

- **url**: a string value that identifies the extension
- **value**: the value of the extension ([any valid type](datatypes.html#open) for [extension.value\[x\]](extensibility.html)

**Return Value:** An extension with the specified properties.

------------------------------------------------------------------------



#### 2.1.9.3.3 %factory.Identifier(system, value, use, type) : Identifier

Creates an identifier with the given properties: `%factory.Identifier('urn:ietf:rfc:3986', 'urn:oid:1.2.3.4.5', 'official')`.

Parameters:

- **system**: a string value that goes in Identifier.system
- **value**: a string value that goes in Identifier.value
- **use**: a string value that goes in Identifier.use
- **type**: a CodeableConcept that goes in Identifier.type

**Return Value:** An identifier with the specified properties .

------------------------------------------------------------------------



#### 2.1.9.3.4 %factory.HumanName(family, given, prefix, suffix, text, use) : HumanName

Create a human name with the given properties: `%factory.HumanName('Smith', 'Julia', {}, {}, 'Julia Smith')`.

Parameters:

- **family**: a string value that goes in HumanName.system
- **given**: a collection of string values that goes in HumanName.given
- **prefix**: a string value that goes in HumanName.prefix
- **suffix**: a string value that goes in HumanName.suffix
- **text**: a string value that goes in HumanName.text
- **use**:a string value that goes in HumanName.use

**Return Value:** a HumanName.

------------------------------------------------------------------------



#### 2.1.9.3.5 %factory.ContactPoint(system, value, use) : ContactPoint

Creates a ContactPoint: `%factory.ContactPoint('email', 'coyote@acme.com', 'work')`

Parameters:

- **system**: a string value that goes in ContactPoint.system
- **value**: a string value that goes in ContactPoint.value
- **use**: a string value that goes in ContactPoint.use

**Return Value:** a ContactPoint.

------------------------------------------------------------------------



#### 2.1.9.3.6 %factory.Address(line, city, state, postalCode, country, use, type) : Address

Creates an Address: `%factory.Address('5 Nowhere Road', 'coyote@acme.com', 'EW', '0000', {}, 'home', 'physical')`

Parameters:

- **line**: a collection of string values that goes in Address.line
- **city**: a string value that goes in Address.city
- **state**: a string value that goes in Address.state
- **postalCode**: a string value that goes in Address.postalCode
- **country**: a string value that goes in Address.country
- **use**: a string value that goes in Address.use
- **type**: a string value that goes in Address.type

**Return Value:** An address.

------------------------------------------------------------------------



#### 2.1.9.3.7 %factory.Quantity(system, code, value, unit) : Quantity

Creates a Quantity: `%factory.Quantity('http://unitsofmeasure.org', 'mg/dL', '5.03', 'mg/dL')`

Parameters:

- **system**: a string value that goes in Quantity.system
- **code**: a string value that goes in Quantity.code
- **value**: a string or decimal value that goes in Quantity.value
- **unit**: a string value that goes in Quantity.unit

**Return Value:** a Quantity.

------------------------------------------------------------------------



#### 2.1.9.3.8 %factory.Coding(system, code, display, version) : Coding

Creates a Coding: `%factory.Coding('http://loinc.org', '1234-5, 'An example test', '1.02')`

Parameters:

- **system**: a string value that goes in Coding.system
- **code**: a string value that goes in Coding.code
- **display**: a string value that goes in Coding.display
- **version**: a string value that goes in Coding.version

**Return Value:** A coding.

------------------------------------------------------------------------



#### 2.1.9.3.9 %factory.CodeableConcept(coding, text) : CodeableConcept

Creates a CodeableConcept: `%factory.CodeableConcept(%factory.Coding(...), "Example Test")`

Parameters:

- **coding**: a collection of Coding that goes in CodeableConcept.coding
- **text**: a string value that goes in CodeableConcept.text

**Return Value:** a CodeableConcept.

For the general type constructors, all the parameters are mandatory. Note that since all variables / outputs in FHIRPath are collections, all the parameters are inherently collections, but when the underlying property referred to is a singleton element, the collection cannot contain more than one item. Use the value `{}` if there is no value to provide.

------------------------------------------------------------------------



#### 2.1.9.3.10 %factory.create(type) : {type}

Create an instance of the named type: `%factory.create(SampledData)`

Parameters:

- **type**: a value that is the type to create. This is a FHIRPath type specifier, and the default namespace is 'FHIR'

**Return Value:** an instance of the named type.

------------------------------------------------------------------------



#### 2.1.9.3.11 %factory.withExtension(instance : T, url : string, value) : T

Add an extension, and return the new type: `%factory.withExtension(%factory.create(Money), 'http:/acme.com/extension/example', %factory.code('test'))`

Parameters:

- **instance**: The instance to add the URL to
- **url**: a string value that goes in Extension.url
- **value**: the value of the extension

**Return Value:** A copy of the instance of the type with the extension added. Extensions that already exist with the same url are not removed.

------------------------------------------------------------------------



#### 2.1.9.3.12 %factory.withProperty(instance : T, name : string, value) : T

Set a property value, and return the new type: `%factory.withProperty(%factory.create(Address), 'http:/acme.com/extension/example', %factory.create(Period))`

Parameters:

- **instance**: The instance to set the property on
- **name**: a string value that identifies the property to set
- **value**: the value of the property

**Return Value:** A copy of the instance of the type with the named property set. Any existing value(s) for the named property will be deleted.



### 2.1.9.4 Terminology Service API

In order to support terminological reasoning in FHIRPath statements, FHIR defines a general %terminologies object that FHIRPath implementations should make available. Calls to this object are passed through a [standard FHIR terminology service](terminology-service.html).

Summary:

``` fhirpath
%terminologies : TerminologyServer // default terminology server (application controls context)
%terminologies.at(url) : TerminologyServer // terminology server at specified address

%terminologies.expand(valueSet, params) : ValueSet
%terminologies.lookup(coded, params) : Parameters
%terminologies.validateVS(valueSet, coded, params) : Parameters
%terminologies.validateCS(codeSystem, coded, params) : Parameters
%terminologies.subsumes(system, coded1, coded2, params) : code
%terminologies.translate(conceptMap, code, params) : Parameters
```

For all these functions, if any of the parameters are empty, or a collection with more than one value, or one or more of the parameters are not valid, the return value is empty.

------------------------------------------------------------------------



#### 2.1.9.4.1 %terminologies.at(url) : TerminologyServer

Get a terminology server object pointing at a particular server. Note: The %terminologies object points to the default terminology server as specified by the application evaluating the FHIRPath.

Parameters:

- **url**: A URL that points to a FHIR RESTful API.

**Return Value:** A terminology server that points at the specified URL. No errors - they will come when/if the terminology server object is used.

------------------------------------------------------------------------



#### 2.1.9.4.2 %terminologies.expand(valueSet, params) : ValueSet

This calls the [Terminology Service \$expand](terminology-service.html#expand) operation ([formal definition](valueset-operation-expand.html)).

Parameters:

- **valueSet**: either an actual [ValueSet](valueset.html), or a [canonical URL](references.html#canonical) reference to a value set.
- **params**: a URL encoded string with other parameters for the expand operation (e.g., 'displayLanguage=en&activeOnly=true')

**Return Value:** a [ValueSet](valueset.html) with an expansion. If an error occurs, the return value is empty.

------------------------------------------------------------------------

------------------------------------------------------------------------



#### 2.1.9.4.3 %terminologies.lookup(coded, params) : Parameters

This calls the [Terminology Service \$lookup](terminology-service.html#lookup) operation ([formal definition](codesystem-operation-lookup.html)).

Parameters:

- **coded**: either a [Coding](datatypes.html#coding), a [CodeableConcept](datatypes.html#CodeableConcept), or a resource element that is a [code](datatypes.html#code)
- **params**: a URL encoded string with other parameters for the lookup operation (e.g., 'date=2011-03-04&displayLanguage=en')

**Return Value:**

------------------------------------------------------------------------



#### 2.1.9.4.4 %terminologies.validateVS(valueSet, coded, params) : Parameters

This calls the [Terminology Service \$validate-code](terminology-service.html#expand) operation on a value set ([formal definition](valueset-operation-validate-code.html)).

Parameters:

- **valueSet**: either an actual [ValueSet](valueset.html), or a [canonical URL](references.html#canonical) reference to a value set.
- **coded**: either a [Coding](datatypes.html#coding), a [CodeableConcept](datatypes.html#CodeableConcept), or a resource element that is a [code](datatypes.html#code)
- **params**: a URL encoded string with other parameters for the validate-code operation (e.g., 'date=2011-03-04&displayLanguage=en')

**Return Value:** A [Parameters](parameters.html) resource with the results of the validation operation.

------------------------------------------------------------------------



#### 2.1.9.4.5 %terminologies.validateCS(codeSystem, coded, params) : Parameters

This calls the [Terminology Service \$validate-code](terminology-service.html#expand) operation on a code system ([formal definition](codesystem-operation-validate-code.html)).

Parameters:

- **codeSystem**: either an actual [CodeSystem](codesystem.html), or a [canonical URL](references.html#canonical) reference to a code system.
- **coded**: either a [Coding](datatypes.html#coding), a [CodeableConcept](datatypes.html#CodeableConcept), or a resource element that is a [code](datatypes.html#code)
- **params**: a URL encoded string with other parameters for the validate-code operation (e.g., 'date=2011-03-04&displayLanguage=en')

**Return Value:** A [Parameters](parameters.html) resource with the results of the validation operation.

------------------------------------------------------------------------



#### 2.1.9.4.6 %terminologies.subsumes(system, coded1, coded2, params) : code

This calls the [Terminology Service \$subsumes](terminology-service.html#subsumes) operation ([formal definition](codesystem-operation-subsumes.html)).

Parameters:

- **system**: the URI of a code system within which the subsumption testing occurs
- **coded1**: A [Coding](datatypes.html#coding) or a resource element that is a [code](datatypes.html#code)
- **coded2**: A [Coding](datatypes.html#coding) or a resource element that is a [code](datatypes.html#code)
- **params**: a URL encoded string with other parameters for the validate-code operation (e.g., 'version=2014-05-06')

**Return Value:** a code as specified for the subsumes operation.

------------------------------------------------------------------------



#### 2.1.9.4.7 %terminologies.translate(conceptMap, coded, params) : Parameters

This calls the [Terminology Service \$translate](terminology-service.html#translate) operation ([formal definition](conceptmap-operation-translate.html)).

Parameters:

- **conceptMap**: either an actual [ConceptMap](conceptmap.html), or a [canonical URL](references.html#canonical) reference to a value set.
- **coded**: The source to translate: a [Coding](datatypes.html#coding) or a resource element that is a [code](datatypes.html#code)
- **params**: a URL encoded string with other parameters for the validate-code operation (e.g., 'source=http://acme.org/valueset/23&target=http://acme.org/valueset/23')

**Return Value:** A [Parameters](parameters.html) resource with the results of the translation operation.



### 2.1.9.5 General Service API

In order to support interaction with a server in FHIRPath statements, FHIR defines a general %server object that FHIRPath implementations should make available. Calls to this object are passed through a [FHIR RESTful framework](http.html).

Summary:

``` fhirpath
%server : Server // default server (application controls context)
%server.at(url) : Server // server at specified address

%server.read(type, id) : Resource
%server.create(resource) : Resource
%server.update(resource) : Resource
%server.delete(resource) : boolean
%server.patch(parameters) : Resource
%server.search(doPost, parameters) : Bundle
%server.capabilities(mode) : Resource
%server.validate(resource, mode, parameters) : OperationOutcome
%server.transform(source, content) : Resource
%server.everything(type, id, parameters) : Bundle
%server.apply(resource, subject, parameters) : Bundle
```

------------------------------------------------------------------------



#### 2.1.9.5.1 %server.at(url) : Server

Get a server object pointing at a particular server. Note: The %server object points to the default server as specified by the application evaluating the FHIRPath.

Parameters:

- **url**: A URL that points to a FHIR RESTful API.

**Return Value:** A server that points at the specified URL. No errors - they will come when/if the server object is used.

------------------------------------------------------------------------



#### 2.1.9.5.2 %server.read(type, id) : Resource

Get a resource from the server.

Parameters:

- **type**: The type of the resource to read.
- **id**: The id of the resource to read.

**Return Value:** The resource at type/id. If not found, the return value is empty.

------------------------------------------------------------------------



#### 2.1.9.5.3 %server.create(resource) : Resource

Create a resource on the server.

Parameters:

- **resource**: The resource to create. If the resource has an id, it will be ignored.

**Return Value:** The resource after it was stored. If the create operation failed, the return value is empty.

------------------------------------------------------------------------



#### 2.1.9.5.4 %server.update(resource) : Resource

Store a resource on the server.

Parameters:

- **resource**: The resource to create. The resource must have an id.

**Return Value:** The resource after it was stored. If the create operation failed, the return value is empty.

------------------------------------------------------------------------



#### 2.1.9.5.5 %server.delete(resource) : boolean

Delete a resource on the server.

Parameters:

- **resource**: The resource to delete (must have an id).

**Return Value:** true if the resource was deleted, or false.

------------------------------------------------------------------------



#### 2.1.9.5.6 %server.search(doPost, parameters) : Bundle

Perform a search on the server.

Parameters:

- **doPost**: A boolean value - true to use a POST, false to use a GET
- **parameters**: A parameters resource, or a string with URL parameters (name=value&etc.)

**Return Value:** A bundle with the search results. If the search fails, the return value is empty.

------------------------------------------------------------------------



#### 2.1.9.5.7 %server.patch(parameters) : Resource

Perform a patch operation on the server.

Parameters:

- **parameters**: A parameters resource for [FHIRPath Patch](fhirpatch.html)

**Return Value:** The resource after the patch. If the patch fails, the return value is empty.

------------------------------------------------------------------------



#### 2.1.9.5.8 %server.capabilities(mode) : Resource

Get the capabilities from the server

Parameters:

- **mode**: Optional: the mode to fetch.

**Return Value:** The resource returned (CapabilitiesStatement or TerminologyCapabilities resource). If not available, the return value is empty.

------------------------------------------------------------------------



#### 2.1.9.5.9 %server.validate(resource, mode, parameters) : OperationOutcome

Validate a resource on the server.

Parameters:

- **resource**: The resource to validate.
- **mode**: how to validate - see [Validation Operation](resource-operation-validate.html).
- **parameters**: A parameters resource, or a string with URL parameters (name=value&etc.)

**Return Value:** An operation outcome with issues. If the validation couldn't be performed, the return value is empty.

------------------------------------------------------------------------



#### 2.1.9.5.10 %server.transform(source, content) : Resource

Run the \$transform operation on the server.

Parameters:

- **source**: The structure map to use.
- **content**: The resource to convert (often a binary)

**Return Value:** The resource returned from the transform. If the transform fails, the return value is empty.

------------------------------------------------------------------------



#### 2.1.9.5.11 %server.everything(type, id, parameters) : Bundle

Get a resource from the server.

Parameters:

- **type**: The type of the resource to read.
- **id**: The id of the resource to read.
- **parameters**: A parameters resource, or a string with URL parameters (name=value&etc.)

**Return Value:** The Bundle for type/id. If not available, the return value is empty.

------------------------------------------------------------------------



#### 2.1.9.5.12 %server.apply(resource, subject, parameters) : Bundle

Get a resource from the server.

Parameters:

- **resource**: The resource to drive the \$apply operation (PlanDefinition, ActivityDefinition).
- **subject**: The subject to apply to - can be a resource, or a string containing type/id for the subject.
- **parameters**: A parameters resource, or a string with URL parameters (name=value&etc.)

**Return Value:** The bundle from \$apply. If the operation fails, the return value is empty.









FHIR ®© HL7.org 2011+. FHIR R6 hl7.fhir.core#6.0.0-ballot3 generated on Wed, Dec 17, 2025 09:55+0000.  
<span style="color: #FFFF77"> Links: <a href="http://hl7.org/fhir/search.cfm" style="color: #b8dcf9">Search</a> \| <a href="history.html" style="color: #b8dcf9">Version History</a> \| <a href="toc.html" style="color: #b8dcf9">Contents</a> \| <a href="help.html" style="color: #b8dcf9">Glossary</a> \| <a href="qa.html" style="color: #ffffff">QA</a> \| <a href="https://www.fhir.org/perl/htmldiff.pl?oldfile=http%3A%2F%2Fhl7.org%2Ffhir%2FR4%2Ffhirpath.html&amp;newfile=http%3A%2F%2Fbuild.fhir.org%2Ffhirpath.html" style="color: #b8dcf9">Compare to R4</a> \| <a href="https://www.fhir.org/perl/htmldiff.pl?oldfile=http%3A%2F%2Fhl7.org%2Ffhir%2FR5%2Ffhirpath.html&amp;newfile=http%3A%2F%2Fbuild.fhir.org%2Ffhirpath.html" style="color: #b8dcf9">Compare to R5</a> \| <a href="https://www.fhir.org/perl/htmldiff.pl?oldfile=http%3A%2F%2Fhl7.org%2Ffhir%2F6.0.0-ballot3%2Ffhirpath.html&amp;newfile=http%3A%2F%2Fbuild.fhir.org%2Ffhirpath.html" style="color: #b8dcf9">Compare to Last Ballot</a> \| <a href="license.html" rel="license" style="color: #b8dcf9"><img src="cc0.png" style="border-style: none;" alt="CC0" /></a> \| <a href="https://jira.hl7.org/secure/CreateIssueDetails!init.jspa?pid=10405&amp;issuetype=10600&amp;customfield_11302=FHIR-core&amp;customfield_11808=R5&amp;customfield_10612=http://build.fhir.org/fhirpath.html" style="color: #b8dcf9" target="_blank">Propose a change</a> </span>



