module [
    start,
    finish,
    field,
    fieldWithRenamedParams,
    object,
    objectWithRenamedParams,
]

# import json.Json
import Query.Base exposing [
    buildQueryStr,
    QueryField,
    QueryObjectField,
    # QuerySelector,
    GraphQLParseError,
    QuerySelectorField,
]

QuerySelectorBuilder data decodeData params encodeParams := {
    fields : List QuerySelectorField,
    # TODO: make List U8
    paramSelector : params -> encodeParams,
    parser : decodeData -> Result data GraphQLParseError,
}

start : state -> QuerySelectorBuilder state decodeData encodeParams encodeParams
start = \parser ->
    @QuerySelectorBuilder {
        fields: [],
        paramSelector: \params -> params,
        parser: \_data -> Ok parser,
    }

# finish : QuerySelectorBuilder decodeData decodeData params encodeParams, Str -> QuerySelector decodeData params where decodeData implements Decoding, encodeParams implements Encoding
# finish = \@QuerySelectorBuilder { fields, paramSelector, parser }, queryName ->
#     newParser = \jsonBytes ->
#         decodedData : Result { data : decodeData } GraphQLParseError
#         decodedData =
#             Decode.fromBytes jsonBytes Json.utf8
#             |> Result.mapErr FailedToDecodeJson

#         decodedData
#         |> Result.try \{ data } -> parser data

#     encodeParams = \paramsData ->
#         paramsData
#         |> paramSelector
#         |> Encode.toBytes Json.utf8

#     {
#         queryStr: buildQueryStr { fields, queryName },
#         encodeParams,
#         parser: newParser,
#     }

finish : QuerySelectorBuilder decodeData decodeData params encodeParams, Str -> _ where decodeData implements Decoding, encodeParams implements Encoding
finish = \@QuerySelectorBuilder { fields, paramSelector, parser }, queryName ->
    newParser = \jsonBytes, fmt ->
        decodedData : Result { data : decodeData } GraphQLParseError
        decodedData =
            Decode.fromBytes jsonBytes fmt
            |> Result.mapErr FailedToDecodeJson

        decodedData
        |> Result.try \{ data } -> parser data

    encodeParams = \paramsData, fmt ->
        paramsData
        |> paramSelector
        |> Encode.toBytes fmt

    {
        queryStr: buildQueryStr { fields, queryName },
        encodeParams,
        parser: newParser,
    }

field :
    ({} -> QueryField decodeData fieldData encodeParams)
    -> (QuerySelectorBuilder (fieldData -> state) decodeData params encodeParams -> QuerySelectorBuilder state decodeData params encodeParams)
field = \fieldConfig ->
    fieldWithRenamedParams fieldConfig (\a -> a) (Dict.empty {})

fieldWithRenamedParams :
    ({} -> QueryField decodeData fieldData fieldParams),
    (fieldParams -> encodeParams),
    Dict Str Str
    -> (QuerySelectorBuilder (fieldData -> state) decodeData params encodeParams -> QuerySelectorBuilder state decodeData params encodeParams)
fieldWithRenamedParams = \fieldConfigThunk, _paramsMapper, paramAliases ->
    fieldConfig = fieldConfigThunk {}

    newParams =
        fieldConfig.params
        |> List.map \param ->
            alias =
                paramAliases
                |> Dict.get param.name
                |> Result.withDefault param.name
            { name: param.name, alias, typeName: param.typeName }

    \@QuerySelectorBuilder { parser, paramSelector, fields } ->
        nextParamSelector = \paramsData ->
            paramsData # |> paramsMapper
            |> paramSelector

        nextParser = \data ->
            nextParserStage <- parser data
                |> Result.try

            fieldPath = [fieldConfig.name]
            fieldData <- fieldConfig.resolver data
                |> Result.mapErr \error -> FieldError { path: fieldPath, error }
                |> Result.try

            Ok (nextParserStage fieldData)

        @QuerySelectorBuilder {
            paramSelector: nextParamSelector,
            fields: List.append fields (Simple { name: fieldConfig.name, params: newParams }),
            parser: nextParser,
        }

object :
    ({} -> QueryObjectField decodeData childDecodeData fields encodeParams),
    (fields -> QuerySelectorBuilder childData childDecodeData params encodeParams)
    -> (QuerySelectorBuilder (childData -> state) decodeData params encodeParams -> QuerySelectorBuilder state decodeData params encodeParams)
object = \objectField, objectFieldParserBuilder ->
    objectWithRenamedParams objectField (\a -> a) (Dict.empty {}) objectFieldParserBuilder

objectWithRenamedParams :
    ({} -> QueryObjectField decodeData childDecodeData fields fieldParams),
    (fieldParams -> encodeParams),
    Dict Str Str,
    (fields -> QuerySelectorBuilder childData childDecodeData params encodeParams)
    -> (QuerySelectorBuilder (childData -> state) decodeData params encodeParams -> QuerySelectorBuilder state decodeData params encodeParams)
objectWithRenamedParams = \objectFieldThunk, _paramsMapper, paramAliases, objectFieldParserBuilder ->
    objectField = objectFieldThunk {}

    newParams =
        objectField.params
        |> List.map \param ->
            alias =
                paramAliases
                |> Dict.get param.name
                |> Result.withDefault param.name
            { name: param.name, alias, typeName: param.typeName }

    (@QuerySelectorBuilder objectBuilder) = objectFieldParserBuilder (objectField.fields {})

    \@QuerySelectorBuilder { parser, paramSelector, fields } ->
        nextParamSelector = \paramsData ->
            paramsData
            |> paramSelector

        nextParser = \data ->
            nextParserStage <- parser data
                |> Result.try
            # TODO: set the error path correctly
            objectDecodeData <- objectField.resolver data
                |> Result.mapErr \error -> FieldError { path: [objectField.name], error }
                |> Result.try
            objectData <- objectBuilder.parser objectDecodeData
                |> Result.try

            Ok (nextParserStage objectData)

        @QuerySelectorBuilder {
            parser: nextParser,
            paramSelector: nextParamSelector,
            # TODO: set params
            fields: fields
            |> List.append (Object { name: objectField.name, params: newParams, children: objectBuilder.fields }),
        }
