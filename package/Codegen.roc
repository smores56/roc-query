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
    [renderModuleHeader schema]
    |> List.concat (List.map schema.queryRoot.fields renderQueryRootFieldBlock)
    |> List.concat (List.map schema.outputObjects renderOutputObjectBlock)
    |> List.intersperse [""]
    |> List.join
    |> List.append ""
    |> Str.joinWith "\n"

renderModuleHeader : Schema -> List Str
renderModuleHeader = \schema ->
    exportLines =
        schema.queryRoot.fields
        |> List.map \field ->
            "    $(queryRootFieldName field),"

    ["module ["]
    |> List.concat exportLines
    |> List.concat [
        "]",
        "",
        "import json.Option exposing [Option]",
        "import rq.Query.Base exposing [QueryField, QueryObjectField]",
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

        "{ $(paramTypes) }a"

queryRootFieldName : SchemaObjectField -> Str
queryRootFieldName = \{ name } -> "query$(capitalize name)"

objectFieldName : Str -> Str
objectFieldName = \objectName ->
    "$(decapitalize objectName)Object"

renderFieldTypeDefinition : SchemaObjectField -> Str
renderFieldTypeDefinition = \field ->
    when getNameOfObjectFromType field.type is
        Ok _objectName ->
            paramsType = field.parameters |> renderParamsType
            "{} -> QueryObjectField _ _ _ $(paramsType)"

        Err NotAnObject ->
            rocTypeName = renderNullableRocType field.type
            "{} -> QueryField _ $(rocTypeName) _"

renderFieldBlock : { field : SchemaObjectField, level : [Root, Child] } -> List Str
renderFieldBlock = \{ field, level } ->
    fieldDefinitionLines =
        when level is
            Child -> ["$(field.name): \\{} -> {"]
            Root ->
                objectName = queryRootFieldName field
                [
                    "$(objectName) : $(renderFieldTypeDefinition field)",
                    "$(objectName) = \\{} -> {",
                ]

    objectFieldsBlock =
        when getNameOfObjectFromType field.type is
            Ok objectName -> ["    fields: $(objectFieldName objectName),"]
            Err NotAnObject -> []

    paramsBlock =
        renderParamsBlock field.parameters
        |> List.map \line ->
            indentTimes line 1

    fieldDefinitionLines
    |> List.append "    name: \"$(field |> .name)\","
    |> List.concat paramsBlock
    |> List.append "    resolver: \\{ $(field |> .name) } -> Ok $(field |> .name),"
    |> List.concat objectFieldsBlock
    |> List.append "}$(if level == Root then "" else ",")"
    |> List.map \line ->
        indentAmount = if level == Child then 1 else 0
        indentTimes line indentAmount

renderQueryRootFieldBlock : SchemaObjectField -> List Str
renderQueryRootFieldBlock = \field -> renderFieldBlock { field, level: Root }

renderOutputObjectFieldBlock : SchemaObjectField -> List Str
renderOutputObjectFieldBlock = \field -> renderFieldBlock { field, level: Child }

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
                |> List.map \field -> "$(field.name) : $(renderFieldTypeDefinition field),"
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

capitalize = \text ->
    chars = Str.toUtf8 text
    firstChar =
        List.first chars
        |> Result.withDefault 0

    updatedChars =
        if firstChar >= 'a' && firstChar <= 'z' then
            capitalizeDiff = 'a' - 'A'
            chars |> List.set 0 (firstChar - capitalizeDiff)
        else
            chars

    Str.fromUtf8 updatedChars
    |> Result.withDefault ""

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
