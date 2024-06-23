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
    CustomScalar,
    SchemaEnum,
    NullableDataType,
    SchemaInputObject,
    SchemaOutputObject,
    SchemaObjectField,
    SchemaFieldParameter,
]
import Parse.Utils exposing [
    parseLowerCamelCaseWord,
    parseUpperCamelCaseWord,
    # parseScreamingSnakeCaseWord,
    dropCharsWhile,
    isWhitespace,
    parseByte,
    fromUtf8Unchecked,
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
    UnexpectedSuffix Str,
]

PartialSchema : {
    customScalars : List CustomScalar,
    enums : List SchemaEnum,
    unions : List SchemaUnion,
    inputObjects : List SchemaInputObject,
    outputObjects : List SchemaOutputObject,
}

ParseSchemaAction : [
    ParseEnum Str (List Str),
    ParseInputObject Str (List SchemaFieldParameter),
    ParseOutputObject Str (List SchemaObjectField),
    SearchForEntity,
]

parseSchema : List Str -> Result Schema _
parseSchema = \rows ->
    emptySchema : PartialSchema
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
        |> List.walkWithIndexUntil (emptySchema, Ok startingAction) \(schema, actionResult), row, rowNumber ->
            when actionResult is
                Err err -> Break (schema, Err err)
                Ok action ->
                    trimmedRow = Str.trimStart row
                    when attemptToParseRowOfSchema schema action trimmedRow is
                        Ok (nextSchema, nextAction) ->
                            Continue (nextSchema, Ok nextAction)

                        Err error ->
                            Break (schema, Err { row, rowNumber, error })

    when resultAction is
        Err context -> Err (RowIssue context)
        Ok (ParseEnum name _variants) -> Err (UnfinishedEnum name)
        Ok (ParseInputObject name _fields) -> Err (UnfinishedInputObject name)
        Ok (ParseOutputObject name _fields) -> Err (UnfinishedOutputObject name)
        Ok SearchForEntity ->
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
                queryRoot,
                mutationRoot,
                subscriptionRoot: {
                    name: "Subscription",
                    annotations: [],
                    fields: [],
                },
            }

attemptToParseRowOfSchema : PartialSchema, ParseSchemaAction, Str -> Result (PartialSchema, ParseSchemaAction) _
attemptToParseRowOfSchema = \schema, action, row ->
    when action is
        SearchForEntity ->
            searchForEntityStart schema row

        ParseInputObject name fields ->
            parseInputObjectFieldRow { schema, name, fields, row }
            |> Result.mapErr FailedToParseInputObject

        ParseOutputObject name fields ->
            parseOutputObjectFieldRow { schema, name, fields, row }
            |> Result.mapErr FailedToParseOutputObject

        ParseEnum name variants ->
            parseEnumVariantRow { schema, name, variants, row }
            |> Result.mapErr FailedToParseEnum

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
                objectName <- parseEntityNameFromFirstRow after
                    |> Result.try
                Ok (schema, ParseInputObject objectName [])

            Ok { before: "type", after } ->
                objectName <- parseEntityNameFromFirstRow after
                    |> Result.try
                Ok (schema, ParseOutputObject objectName [])

            Ok { before: "enum", after } ->
                enumName <- parseEntityNameFromFirstRow after
                    |> Result.try
                Ok (schema, ParseEnum enumName [])

            _otherwise -> Err NoEntityFound

parseEntityNameFromFirstRow = \restOfFirstRow ->
    trimmedRow = Str.trimStart restOfFirstRow
    (entityName, afterEntityName) <- parseUpperCamelCaseWord (Str.toUtf8 trimmedRow)
        |> Result.mapErr InvalidName
        |> Result.try

    if dropCharsWhile afterEntityName isWhitespace == ['{'] then
        Ok entityName
    else
        Err (InvalidTextAfterName afterEntityName)

parseInputObjectFieldRow : _ -> Result (PartialSchema, ParseSchemaAction) _
parseInputObjectFieldRow = \{ schema, name, fields, row } ->
    if Str.isEmpty row || Str.startsWith row "#" then
        Ok (schema, ParseInputObject name fields)
    else if Str.trim row == "}" then
        inputObject = { name, fields, annotations: [] }
        Ok (
            { schema & inputObjects: List.append schema.inputObjects inputObject },
            SearchForEntity,
        )
    else
        (fieldName, afterFieldName) <- parseLowerCamelCaseWord (Str.toUtf8 row)
            |> Result.mapErr InvalidFieldName
            |> Result.try
        afterFieldTrimmed = dropCharsWhile afterFieldName isWhitespace

        afterColon <- parseByte afterFieldTrimmed ':'
            |> Result.mapErr \ByteNotFound -> ColonIsMissingFromRow
            |> Result.try
        afterColonTrimmed = dropCharsWhile afterColon isWhitespace

        (dataType, afterDataType) <- parseDataType afterColonTrimmed
            |> Result.mapErr InvalidDataType
            |> Result.try

        if List.isEmpty afterDataType then
            objectField = {
                name: fieldName,
                description: "",
                # TODO: parse
                default: Err NoDefault,
                type: dataType,
            }

            Ok (
                schema,
                ParseInputObject name (List.append fields objectField),
            )
        else
            Err X

parseOutputObjectFieldRow : _ -> Result (PartialSchema, ParseSchemaAction) _
parseOutputObjectFieldRow = \{ schema, name, fields, row } ->
    if Str.isEmpty row || Str.startsWith row "#" then
        Ok (schema, ParseOutputObject name fields)
    else if Str.trim row == "}" then
        outputObject = { name, fields, annotations: [] }
        Ok (
            { schema & outputObjects: List.append schema.outputObjects outputObject },
            SearchForEntity,
        )
    else
        (fieldName, afterFieldName) <- parseLowerCamelCaseWord (Str.toUtf8 row)
            |> Result.mapErr InvalidFieldName
            |> Result.try
        afterFieldTrimmed = dropCharsWhile afterFieldName isWhitespace

        parametersResult =
            when afterFieldTrimmed is
                ['(', .. as afterParensStart] ->
                    parseParameters afterParensStart

                _ -> Ok ([], afterFieldTrimmed)

        (parameters, afterParams) <- parametersResult
            |> Result.try
        afterParamsAndSpaces = dropCharsWhile afterParams isWhitespace

        afterColon <- parseByte afterParamsAndSpaces ':'
            |> Result.mapErr \ByteNotFound -> ColonIsMissingFromRow
            |> Result.try
        afterColonTrimmed = dropCharsWhile afterColon isWhitespace

        (dataType, afterDataType) <- parseDataType afterColonTrimmed
            |> Result.mapErr InvalidDataType
            |> Result.try

        if List.isEmpty afterDataType then
            objectField = {
                name: fieldName,
                description: "",
                parameters,
                type: dataType,
            }

            Ok (
                schema,
                ParseOutputObject name (List.append fields objectField),
            )
        else
            Err X

parseParameters : List U8 -> Result (List SchemaFieldParameter, List U8) _
parseParameters = \allBytes ->
    takeParam = \paramBytes ->
        (paramName, afterParamName) <- parseLowerCamelCaseWord paramBytes
            |> Result.mapErr InvalidParamName
            |> Result.try
        afterParamAndSpaces = dropCharsWhile afterParamName isWhitespace
        afterColon <- parseByte afterParamAndSpaces ':'
            |> Result.mapErr \ByteNotFound -> NoColonAfterParamName
            |> Result.try
        afterColonAndSpaces = dropCharsWhile afterColon isWhitespace
        (paramType, afterParamType) <- parseDataType afterColonAndSpaces
            |> Result.mapErr ParamMissingType
            |> Result.try

        Ok (
            { name: paramName, description: "", type: paramType, default: Err NoDefault },
            dropCharsWhile afterParamType isWhitespace,
        )

    takeRemainingParams = \remainingBytes ->
        when remainingBytes is
            [')', .. as afterParens] -> Ok ([], afterParens)
            _ ->
                afterComma <- parseByte remainingBytes ','
                    |> Result.mapErr \ByteNotFound -> NoCommaAfterParam
                    |> Result.try
                afterCommaAndSpaces = dropCharsWhile afterComma isWhitespace

                (param, afterParam) <- takeParam afterCommaAndSpaces
                    |> Result.try

                (restOfParams, afterRestOfParams) <- takeRemainingParams afterParam
                    |> Result.try

                Ok (List.concat [param] restOfParams, afterRestOfParams)

    (firstParam, afterFirstParam) <- takeParam allBytes
        |> Result.mapErr FirstParam
        |> Result.try

    (otherParams, afterOtherParams) <- takeRemainingParams afterFirstParam
        |> Result.mapErr OtherParams
        |> Result.try

    Ok (List.concat [firstParam] otherParams, afterOtherParams)

parseEnumVariantRow : _ -> Result (PartialSchema, ParseSchemaAction) ParseEnumError
parseEnumVariantRow = \{ schema, name, variants, row } ->
    if Str.isEmpty row || Str.startsWith row "#" then
        Ok (schema, ParseEnum name variants)
    else if Str.trim row == "}" then
        enum = { name, variants }
        Ok (
            { schema & enums: List.append schema.enums enum },
            SearchForEntity,
        )
    else
        # TODO: turn this back on
        # (variantName, afterVariantName) <- parseScreamingSnakeCaseWord (Str.toUtf8 row)
        #     |> Result.mapErr InvalidEnumVariantName
        #     |> Result.try
        # remainingText = dropCharsWhile afterVariantName isWhitespace
        # if List.isEmpty remainingText then
        #     Ok (schema, ParseEnum name (List.append variants variantName))
        # else
        #     Err (UnexpectedSuffix (fromUtf8Unchecked remainingText))
        Ok (schema, ParseEnum name (List.append variants (Str.trim row)))

parseUnion : Str -> Result SchemaUnion ParseUnionError
parseUnion = \row ->
    parseRemainingMembers = \restOfText ->
        trimmedRest = dropCharsWhile restOfText isWhitespace
        if List.isEmpty trimmedRest then
            Ok []
        else
            when List.splitFirst trimmedRest '|' is
                Ok { before: [], after } ->
                    trimmedAfter = dropCharsWhile after isWhitespace
                    (memberName, afterMemberName) <- parseUpperCamelCaseWord trimmedAfter
                        |> Result.mapErr InvalidMemberName
                        |> Result.try
                    restOfMembers <- parseRemainingMembers afterMemberName
                        |> Result.try

                    Ok (List.concat [memberName] restOfMembers)

                _otherwise -> Err (UnexpectedSuffix (fromUtf8Unchecked trimmedRest))

    trimmedRow = Str.trimStart row
    (unionName, afterUnionName) <- parseUpperCamelCaseWord (Str.toUtf8 trimmedRow)
        |> Result.mapErr InvalidName
        |> Result.try
    afterNameTrimmed = dropCharsWhile afterUnionName isWhitespace

    afterEqualsSign <- afterNameTrimmed
        |> parseByte '='
        |> Result.mapErr \ByteNotFound -> NameNotFollowedByEqualsSign
        |> Result.try
    afterEqualsTrimmed = dropCharsWhile afterEqualsSign isWhitespace

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
            ['I', 'n', 't', .. as rest] -> Ok (Int, rest)
            ['F', 'l', 'o', 'a', 't', .. as rest] -> Ok (Float, rest)
            ['B', 'o', 'o', 'l', 'e', 'a', 'n', .. as rest] -> Ok (Float, rest)
            ['[', .. as rest] ->
                (innerType, afterInner) <- parseDataType rest
                    |> Result.try
                when afterInner is
                    [']', .. as afterList] -> Ok (List innerType, afterList)
                    _ -> Err InvalidList

            _ ->
                (name, afterName) <- parseUpperCamelCaseWord chars
                    |> Result.try

                Ok (Object name, afterName)

    (type, leftover) <- parseResult
        |> Result.try

    when leftover is
        ['!', .. as afterLeftover] -> Ok (NotNull type, afterLeftover)
        _ -> Ok (Nullable type, leftover)
