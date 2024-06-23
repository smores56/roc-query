module [generateModule]

import Schema exposing [
    Schema,
    SchemaOutputObject,
    SchemaFieldParameter,
    SchemaObjectField,
    NullableDataType,
]

renderNullableGqlType : NullableDataType -> Str
renderNullableGqlType = \nullableDataType ->
    when nullableDataType is
        Nullable dataType ->
            renderGqlType dataType

        NotNull dataType ->
            renderGqlType dataType |> Str.concat "!"

renderGqlType = \dataType ->
    when dataType is
        ID -> "ID"
        String -> "String"
        Int -> "Int"
        Float -> "Float"
        Boolean -> "Boolean"
        List innerType -> "[$(renderNullableGqlType innerType)]"
        Object innerType -> innerType
        CustomScalar innerType -> innerType

renderNullableRocType : NullableDataType -> Str
renderNullableRocType = \nullableDataType ->
    when nullableDataType is
        Nullable dataType -> "(Option $(renderRocType dataType))"
        NotNull dataType -> renderRocType dataType

renderRocType = \dataType ->
    when dataType is
        ID | String -> "Str"
        Int -> "I64"
        Float -> "F64"
        Boolean -> "Bool"
        List innerType -> "(List $(renderNullableRocType innerType))"
        Object _innerType -> "_"
        # TODO: handle these?
        CustomScalar _innerType -> "_"

getNameOfObjectFromType : NullableDataType -> Result Str [NotAnObject]
getNameOfObjectFromType = \nullableDataType ->
    when nullableDataType is
        Nullable (Object objectName) | NotNull (Object objectName) -> Ok objectName
        Nullable (List listType) | NotNull (List listType) -> getNameOfObjectFromType listType
        _ -> Err NotAnObject

generateModule : { schema : Schema } -> Str
generateModule = \{ schema } ->
    [renderModuleHeader schema] # |> List.concat (List.map schema.queryRoot.fields renderQueryRootFieldBlock)
    |> List.append (renderBuildQueryFunction schema)
    |> List.concat (List.map schema.outputObjects renderOutputObjectBlock)
    |> List.intersperse [""]
    |> List.join
    |> List.append ""
    |> Str.joinWith "\n"

renderBuildQueryFunction : Schema -> List Str
renderBuildQueryFunction = \schema ->
    [
        "buildQuery : Str, (_ -> Query.QuerySelectorBuilder decodeData decodeData params encodeParams { query : {} }) -> Query.QuerySelector decodeData params where decodeData implements Decoding, encodeParams implements Encoding",
        "buildQuery = \\queryName, selectorBuilder ->",
        "    selectorBuilder {",
    ]
    |> List.concat
        (
            schema.outputObjects
            |> List.map \obj ->
                "        $(decapitalize obj.name): $(objectFieldName obj.name) {},"
        )
    |> List.concat [
        "    }",
        "    |> Query.finish queryName",
    ]

renderModuleHeader : Schema -> List Str
renderModuleHeader = \_schema -> [
    "module [buildQuery]",
    "",
    "import json.Option exposing [Option]",
    "import rq.Query",
    "import rq.Utils",
]

renderParamsBlock : List SchemaFieldParameter -> List Str
renderParamsBlock = \params ->
    if List.isEmpty params then
        ["params: [],"]
    else
        paramLines =
            List.map params \param ->
                param
                |> renderParamLine
                |> indentTimes 1

        ["params: ["]
        |> List.concat paramLines
        |> List.append "],"

renderParamLine : SchemaFieldParameter -> Str
renderParamLine = \{ name, type } ->
    typeName = renderNullableGqlType type
    "{ name: \"$(name)\", typeName: \"$(typeName)\", encoder: \\x -> x },"

renderParamsType : List SchemaFieldParameter -> Str
renderParamsType = \params ->
    if List.isEmpty params then
        "{}a"
    else
        paramTypes =
            params
            |> List.map \{ name, type } -> "$(name) : $(renderNullableRocType type)"
            |> Str.joinWith ", "

        "{ $(paramTypes) }*"

objectFieldName : Str -> Str
objectFieldName = \objectName ->
    "$(decapitalize objectName)Object"

renderFieldTypeDefinition : { field : SchemaObjectField, parentObjectName : Str, index : U64 } -> Str
renderFieldTypeDefinition = \{ field, parentObjectName, index } ->
    paramsType = field.parameters |> renderParamsType

    renderCompositeDataType = \type, nameOfObject ->
        when type is
            Option inner -> "(Option $(renderCompositeDataType inner nameOfObject))"
            List inner -> "(List $(renderCompositeDataType inner nameOfObject))"
            Single -> nameOfObject

    when getNameOfObjectFromType field.type is
        Ok objectName ->
            childDataType = "childData$(Num.toStr index)"
            compositeType = renderCompositeDataType (getParserApplicatorNullableType field.type) childDataType

            "Query.QueryObjectField { $(field.name) : $(compositeType) }* ($(compositeType)) $(childDataType) _ $(paramsType) { $(objectFieldName objectName): {} }"

        Err NotAnObject ->
            rocTypeName = renderNullableRocType field.type
            "Query.QueryField { $(field.name) : $(rocTypeName) }* $(rocTypeName) $(paramsType) { $(parentObjectName): {} }"

getParserApplicatorNullableType = \type ->
    when type is
        Nullable innerType -> Option (getParserApplicatorType innerType)
        NotNull innerType -> getParserApplicatorType innerType

getParserApplicatorType = \type ->
    when type is
        List listType -> List (getParserApplicatorNullableType listType)
        ID | String | Int | Float | Boolean | Object _name | CustomScalar _name -> Single

getParserApplicatorFunctionName : NullableDataType -> Str
getParserApplicatorFunctionName = \fieldType ->
    when getParserApplicatorNullableType fieldType is
        Single -> "Utils.trySingle"
        List Single -> "Utils.tryList"
        Option Single -> "Utils.tryOption"
        List (Option Single) -> "Utils.tryListOfOptions"
        Option (List Single) -> "Utils.tryOptionList"
        List (List Single) -> "Utils.tryListList"
        # TODO: handle this more visibly to the user
        _ -> "Utils.trySingle"

renderOutputObjectFieldBlock : SchemaObjectField -> List Str
renderOutputObjectFieldBlock = \field ->
    (fieldDefinitionLine, objectFieldsBlock) =
        when getNameOfObjectFromType field.type is
            Ok objectName ->
                applicatorFunctionName = getParserApplicatorFunctionName field.type
                (
                    "    $(field.name): Query.newObjectField {",
                    [
                        "        applyParserToItems: $(applicatorFunctionName),",
                        "        fields: $(objectFieldName objectName) {},",
                    ],
                )

            Err NotAnObject ->
                (
                    "    $(field.name): Query.newField {",
                    [],
                )

    paramsBlock =
        renderParamsBlock field.parameters
        |> List.map \line ->
            indentTimes line 2

    [fieldDefinitionLine]
    |> List.append "        name: \"$(field.name)\","
    |> List.concat paramsBlock
    |> List.append "        resolver: \\{ $(field.name) } -> Ok $(field.name),"
    |> List.concat objectFieldsBlock
    |> List.append "    },"

renderOutputObjectBlock : SchemaOutputObject -> List Str
renderOutputObjectBlock = \{ name, fields } ->
    objectName = objectFieldName name
    header =
        [
            "$(objectName) : {}",
            "    -> {",
        ]
        |> List.concat
            (
                fields
                |> List.mapWithIndex \field, index ->
                    fieldTypeDefinition = renderFieldTypeDefinition { field, parentObjectName: objectName, index }
                    "$(field.name) : $(fieldTypeDefinition),"
                |> List.map \line -> indentTimes line 2
            )
        |> List.concat [
            "    }",
            "$(objectName) = \\{} -> {",
        ]

    [header]
    |> List.concat (fields |> List.map renderOutputObjectFieldBlock)
    |> List.append ["}"]
    |> List.join

# capitalize = \text ->
#     chars = Str.toUtf8 text
#     firstChar =
#         List.first chars
#         |> Result.withDefault 0

#     updatedChars =
#         if firstChar >= 'a' && firstChar <= 'z' then
#             capitalizeDiff = 'a' - 'A'
#             chars |> List.set 0 (firstChar - capitalizeDiff)
#         else
#             chars

#     Str.fromUtf8 updatedChars
#     |> Result.withDefault ""

decapitalize = \text ->
    chars = Str.toUtf8 text
    firstChar =
        List.first chars
        |> Result.withDefault 0

    updatedChars =
        if firstChar >= 'A' && firstChar <= 'Z' then
            capitalizeDiff = 'a' - 'A'
            chars |> List.set 0 (firstChar + capitalizeDiff)
        else
            chars

    Str.fromUtf8 updatedChars
    |> Result.withDefault ""

indentTimes = \text, amount ->
    indent = " " |> Str.repeat 4
    "$(Str.repeat indent amount)$(text)"

expect
    schema : Schema
    schema = {
        customScalars: [],
        enums: [],
        unions: [],
        inputObjects: [],
        outputObjects: [
            {
                name: "User",
                annotations: [],
                fields: [
                    { name: "name", description: "", parameters: [], type: NotNull String },
                ],
            },
        ],
        queryRoot: {
            name: "Query",
            annotations: [],
            fields: [
                { name: "loggedIn", description: "", parameters: [], type: NotNull Boolean },
                {
                    name: "user",
                    description: "",
                    parameters: [
                        { name: "userId", description: "", type: NotNull Int, default: Err NoDefault },
                    ],
                    type: NotNull (List (NotNull (Object "User"))),
                },
            ],
        },
        mutationRoot: {
            name: "Mutation",
            annotations: [],
            fields: [],
        },
        subscriptionRoot: {
            name: "Subscription",
            annotations: [],
            fields: [],
        },
    }

    generated = generateModule { schema }

    dbg generated

    # ==
    # """
    #  module [
    #      queryLoggedIn,
    #  ]
    #  import Query.Base exposing [QueryField, QueryObjectField]
    # """
    generated != ""
