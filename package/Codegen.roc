module [generateModule]

import Schema exposing [
    Schema,
    SchemaOutputObject,
    SchemaFieldParameter,
    SchemaObjectField,
    NullableDataType,
]

# queryLoggedIn : {} -> QueryField _ Bool _
# queryLoggedIn = \{} -> {
#     name: "loggedIn",
#     params: [],
#     resolver: \{ loggedIn } -> Ok loggedIn,
# }

# queryUser : {} -> QueryObjectField _ _ _ { userId : I64 }a
# queryUser = \{} -> {
#     name: "user",
#     params: [{ name: "userId", typeName: "Int!", encoder: \x -> x }],
#     resolver: \{ user } -> Ok user,
#     fields: testUserObject,
# }

# testUserObject : {}
#     -> {
#         name : {} -> QueryField _ Str _,
#         age : {} -> QueryField _ I64 _,
#         address : {} -> QueryObjectField _ _ _ { primary : Bool }a,
#     }
# testUserObject = \{} -> {
#     name: \{} -> {
#         name: "name",
#         params: [],
#         resolver: \{ name } -> Ok name,
#     },
#     age: \{} -> {
#         name: "age",
#         params: [],
#         resolver: \{ age } -> Ok age,
#     },
#     address: \{} -> {
#         name: "address",
#         params: [
#             { name: "primary", typeName: "Bool!", encoder: \x -> x },
#         ],
#         resolver: \{ address } -> Ok address,
#         fields: testAddressObject,
#     },
# }

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
        "import Query.Base exposing [QueryField, QueryObjectField]",
    ]

renderParamLine : SchemaFieldParameter -> Str
renderParamLine = \{ name, type } ->
    typeName = renderNullableGqlType type
    "{ name: \"$(name)\", typeName: \"$(typeName)\", encoder: \\x -> x },"

renderParamsType : List SchemaFieldParameter -> Str
renderParamsType = \params ->
    paramTypes =
        params
        |> List.map \{ name, type } -> "$(name) : $(renderNullableRocType type)"
        |> Str.joinWith ", "

    "{$(paramTypes)}a"

queryRootFieldName : SchemaObjectField -> Str
queryRootFieldName = \{ name } -> "query$(capitalize name)"

renderQueryRootFieldBlock : SchemaObjectField -> List Str
renderQueryRootFieldBlock = \field ->
    blockName = queryRootFieldName field
    rocTypeName = renderNullableRocType field.type

    (typeDefinitionLine, objectFieldsBlock) =
        when field.type is
            Nullable (Object objectName) | NotNull (Object objectName) ->
                objectFieldName = "$(decapitalize objectName)Object"
                paramsType = field |> .parameters |> renderParamsType
                (
                    "$(blockName) : {} -> QueryObjectField _ _ _ $(paramsType)",
                    ["    fields: $(objectFieldName),"],
                )

            _ ->
                ("$(blockName) : {} -> QueryField _ $(rocTypeName)", [])

    paramsBlock =
        if List.isEmpty field.parameters then
            ["    params: [],"]
        else
            paramLines =
                List.map field.parameters \param ->
                    param
                    |> renderParamLine
                    |> indentTimes 2

            ["    params: ["]
            |> List.concat paramLines
            |> List.append "    ],"

    [
        typeDefinitionLine,
        "$(blockName) = \\{} -> {",
        "    name: \"$(field |> .name)\"",
    ]
    |> List.concat paramsBlock
    |> List.append "    resolver: \\{ $(field |> .name) } -> Ok $(field |> .name),"
    |> List.concat objectFieldsBlock
    |> List.append "}"

# testUserObject : {}
#     -> {
#         name : {} -> QueryField _ Str _,
#         address : {} -> QueryObjectField _ _ _ { primary : Bool }a,
#     }
# testUserObject = \{} -> {
#     name: \{} -> {
#         name: "name",
#         params: [],
#         resolver: \{ name } -> Ok name,
#     },
#     address: \{} -> {
#         name: "address",
#         params: [
#             { name: "primary", typeName: "Bool!", encoder: \x -> x },
#         ],
#         resolver: \{ address } -> Ok address,
#         fields: testAddressObject,
#     },
# }
renderOutputObjectBlock : SchemaOutputObject -> List Str
renderOutputObjectBlock = \{ name, fields: _ } ->
    objectName = "$(decapitalize name)Object"
    header = [
        "$(objectName) : {}",
        "    -> {",
    ]

    []

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
    schema = {
        customScalars: [],
        enums: [],
        unions: [],
        inputObjects: [],
        outputObjects: [],
        queryRoot: {
            name: "Query",
            annotations: [],
            fields: [
                { name: "loggedIn", description: "", parameters: [], type: NotNull Boolean },
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
