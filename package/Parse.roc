module [
    parseSchema,
    ParseSchemaError,
    ParseSchemaErrorType,
    ParseInputObjectError,
    ParseOutputObjectError,
    ParseUnionError,
    ParseEnumError,
]

import Schema exposing [
    Schema,
    SchemaUnion,
    DataType,
    NullableDataType,
]
import Parse.Utils exposing [
    removePrefix,
    parseLowerCamelCaseWord,
    parseUpperCamelCaseWord,
    parseScreamingSnakeCaseWord,
    dropCharsWhile,
    isWhitespace,
    InvalidLowerCamelCaseWord,
    InvalidUpperCamelCaseWord,
    InvalidScreamingSnakeCaseWord,
]

ParseSchemaError : [
    QueryRootNotProvided,
    MutationRootNotProvided,
    UnfinishedEnum Str,
    UnfinishedInputObject Str,
    UnfinishedOutputObject Str,
    RowIssue
        {
            row : Str,
            rowNumber : U64,
            error : ParseSchemaErrorType,
        },
]

ParseSchemaErrorType : [
    NoEntityFound,
    FailedToParseInputObject ParseInputObjectError,
    FailedToParseOutputObject ParseOutputObjectError,
    FailedToParseEnum ParseEnumError,
    FailedToParseUnion ParseUnionError,
]

ParseInputObjectError : []

ParseOutputObjectError : []

ParseUnionError : [
    InvalidName InvalidUpperCamelCaseWord,
    NameNotFollowedByEqualsSign,
    InvalidMemberName InvalidUpperCamelCaseWord,
    UnexpectedSuffix Str,
]

ParseEnumError : [
    InvalidName Str,
    InvalidTextAfterName Str,
    InvalidEnumVariantName InvalidScreamingSnakeCaseWord,
]

parseSchema : List Str -> Result Schema _
parseSchema = \rows ->
    emptySchema = {
        customScalars: [],
        enums: [],
        unions: [],
        inputObjects: [],
        outputObjects: [],
    }
    startingAction = SearchForEntity

    (resultSchema, resultAction) =
        rows
        |> List.walkWithIndexUntil (emptySchema, startingAction) \(schema, action), row, rowNumber ->
            trimmedRow = Str.trimStart row
            when rowParserRenameThis schema action trimmedRow is
                Ok (nextSchema, nextAction) ->
                    Continue (nextSchema, nextAction)

                Err error ->
                    Break (schema, FailedToParse { row, rowNumber, error })

    when resultAction is
        FailedToParse context -> Err (LineErr context)
        ParseEnum name _variants -> Err (UnfinishedEnum name)
        ParseInputObject name fields -> Err (UnfinishedInputObject name)
        ParseOutputObject name _fields -> Err (UnfinishedOutputObject name)
        SearchForEntity ->
            queryRoot <- resultSchema.outputObjects
                |> List.findFirst \object -> object.name == "Query"
                |> Result.mapErr \NotFound -> QueryRootNotProvided
                |> Result.try
            mutationRoot <- resultSchema.outputObjects
                |> List.findFirst \object -> object.name == "Mutation"
                |> Result.mapErr \NotFound -> MutationRootNotProvided
                |> Result.try

            Ok {
                enums: resultSchema.enums,
                unions: resultSchema.unions,
                inputObjects: resultSchema.inputObjects,
                outputObjects: resultSchema.outputObjects,
                customScalars: resultSchema.customScalars,
                query: queryRoot,
                mutation: mutationRoot,
            }

rowParserRenameThis = \schema, action, row ->
    when action is
        SearchForEntity -> searchForEntityStart schema row
        ParseInputObject name fields -> parseInputObjectFieldRow schema name fields row
        ParseOutputObject name fields -> parseOutputObjectFieldRow schema name fields row
        ParseEnum name fields -> parseEnumVariantRow schema name fields row

searchForEntityStart = \schema, row ->
    if Str.isEmpty row then
        Ok (schema, SearchForEntity)
    else
        when Str.splitFirst row " " is
            Ok { before: "union", after } ->
                union <- parseUnion after
                    |> Result.try
                schemaWithUnion = { schema & unions: schema.unions |> List.append union }
                Ok (schemaWithUnion, SearchForEntity)

            Ok { before: "input", after } ->
                objectName <- parseInputObjectNameFromFirstRow after
                    |> Result.try
                Ok (schema, ParseInputObject objectName [])

            Ok { before: "type", after } ->
                objectName <- parseOutputObjectNameFromFirstRow after
                    |> Result.try
                Ok (schema, ParseOutputObject objectName [])

            Ok { before: "enum", after } ->
                enumName <- parseEnumNameFromFirstRow after
                    |> Result.try
                Ok (schema, ParseEnum enumName [])

            _otherwise -> Err NoEntityFound

parseInputObjectNameFromFirstRow = \firstRow ->
    # TODO: error out if "type" isn't followed by space(s)
    trimmedRow = Str.trimStart firstRow
    (objectName, afterObjectName) <- parseUpperCamelCaseWord trimmedRow
        |> Result.mapErr InvalidName
        |> Result.try

    if Str.trim afterObjectName == "{" then
        Ok objectName
    else
        Err (InvalidTextAfterName afterObjectName)

parseOutputObjectNameFromFirstRow = \firstRow ->
    trimmedRow = Str.trimStart firstRow
    (objectName, afterObjectName) <- parseUpperCamelCaseWord trimmedRow
        |> Result.mapErr InvalidName
        |> Result.try

    if Str.trim afterObjectName == "{" then
        Ok objectName
    else
        Err (InvalidTextAfterName afterObjectName)

parseEnumNameFromFirstRow = \firstRow ->
    trimmedRow = Str.trimStart firstRow
    (enumName, afterEnumName) <- parseUpperCamelCaseWord trimmedRow
        |> Result.mapErr InvalidName
        |> Result.try

    if Str.trim afterEnumName == "{" then
        Ok enumName
    else
        Err (InvalidTextAfterName afterEnumName)

parseInputObjectFieldRow = \schema, name, fields, row ->
    if Str.isEmpty row then
        Ok (schema, ParseInputObject name fields)
    else if row |> Str.startsWith "}" then
        inputObject = { name, fields }
        Ok (
            { schema & inputObjects: List.append schema.inputObjects inputObject },
            SearchForEntity,
        )
    else
        (fieldName, afterFieldName) <- parseLowerCamelCaseWord row
            |> Result.mapErr InvalidFieldName
            |> Result.try
        afterFieldTrimmed = Str.trimStart afterFieldName

        afterColon <- removePrefix afterFieldTrimmed ":"
            |> Result.mapErr \PrefixNotPresent -> ColonIsMissingFromRow
            |> Result.try
        afterColonTrimmed = Str.trimStart afterColon

        (dataType, afterDataType) <- parseDataType (Str.toUtf8 afterColonTrimmed)
            |> Result.mapErr InvalidDataType
            |> Result.try

        if List.isEmpty afterDataType then
            objectField = {
                name: fieldName,
                description: "",
                type: dataType,
            }

            Ok (
                schema,
                ParseInputObject name (List.append fields objectField),
            )
        else
            Err X

parseOutputObjectFieldRow = \schema, name, fields, row ->
    if row |> Str.startsWith "}" then
        outputObject = { name, fields }
        Ok (
            { schema & outputObjects: List.append schema.outputObjects outputObject },
            SearchForEntity,
        )
    else
        (fieldName, afterFieldName) <- parseLowerCamelCaseWord row
            |> Result.mapErr InvalidFieldName
            |> Result.try
        afterFieldTrimmed = Str.trimStart afterFieldName

        afterColon <- removePrefix afterFieldTrimmed ":"
            |> Result.mapErr \PrefixNotPresent -> ColonIsMissingFromRow
            |> Result.try
        afterColonTrimmed = Str.trimStart afterColon

        (dataType, afterDataType) <- parseDataType (Str.toUtf8 afterColonTrimmed)
            |> Result.mapErr InvalidDataType
            |> Result.try

        if List.isEmpty afterDataType then
            objectField = {
                name: fieldName,
                description: "",
                type: dataType,
            }

            Ok (
                schema,
                ParseOutputObject name (List.append fields objectField),
            )
        else
            Err X

parseEnumVariantRow = \schema, name, variants, row ->
    if row |> Str.startsWith "}" then
        enum = { name, variants }
        Ok (
            { schema & enums: List.append schema.enums enum },
            SearchForEntity,
        )
    else
        (variantName, afterVariantName) <- parseScreamingSnakeCaseWord row
            |> Result.mapErr InvalidEnumVariantName
            |> Result.try

        remainingText = Str.trim afterVariantName
        if Str.isEmpty remainingText then
            Ok (schema, ParseEnum name (List.append variants variantName))
        else
            Err (UnexpectedSuffix remainingText)

parseUnion : Str -> Result SchemaUnion ParseUnionError
parseUnion = \row ->
    parseRemainingMembers = \restOfText ->
        trimmedRest = Str.trimStart restOfText
        if Str.isEmpty trimmedRest then
            Ok []
        else
            when Str.splitFirst trimmedRest "|" is
                Ok { before: "", after } ->
                    trimmedAfter = Str.trimStart after
                    (memberName, afterMemberName) <- parseUpperCamelCaseWord trimmedAfter
                        |> Result.mapErr InvalidMemberName
                        |> Result.try
                    restOfMembers <- parseRemainingMembers afterMemberName
                        |> Result.try

                    Ok (List.concat [memberName] restOfMembers)

                _otherwise -> Err (UnexpectedSuffix trimmedRest)

    trimmedRow = Str.trimStart row
    (unionName, afterUnionName) <- parseUpperCamelCaseWord trimmedRow
        |> Result.mapErr InvalidName
        |> Result.try
    afterNameTrimmed = Str.trimStart afterUnionName

    afterEqualsSign <- afterNameTrimmed
        |> removePrefix "="
        |> Result.mapErr \PrefixNotPresent -> NameNotFollowedByEqualsSign
        |> Result.try
    afterEqualsTrimmed = Str.trimStart afterEqualsSign

    (firstMember, afterFirstMember) <- parseUpperCamelCaseWord afterEqualsTrimmed
        |> Result.mapErr InvalidName
        |> Result.try
    otherMembers <- parseRemainingMembers afterFirstMember
        |> Result.try

    Ok {
        name: unionName,
        members: List.concat [firstMember] otherMembers,
    }

parseDataType : List U8 -> Result (NullableDataType, List U8) _
parseDataType = \chars ->
    parseResult =
        when chars is
            ['I', 'D', .. as rest] -> Ok (ID, rest)
            ['S', 't', 'r', 'i', 'n', 'g', .. as rest] -> Ok (String, rest)

    (type, leftover) <- parseResult
        |> Result.try

    when leftover is
        ['!', .. as afterLeftover] -> Ok (NotNull type, afterLeftover)
        _ -> Ok (Nullable type, leftover)

