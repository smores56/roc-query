module [
    start,
    finish,
    field,
    object,
    newField,
    newObjectField,
    QuerySelectorBuilder,
    QueryField,
    QueryObjectField,
    QuerySelector,
    QuerySelectorField,
    QuerySelectorParam,
    GraphQLParseError,
    ResolveFieldError,
    QueryFieldParam,
]

import json.Json

QuerySelector data params : {
    queryStr : Str,
    encodeParams : params -> List U8,
    parser : List U8 -> Result data GraphQLParseError,
}

QuerySelectorField : [
    Simple { name : Str, params : List QuerySelectorParam },
    Object { name : Str, params : List QuerySelectorParam, children : List QuerySelectorField },
]

QuerySelectorParam : {
    name : Str,
    alias : Str,
    typeName : Str,
}

GraphQLParseError : [
    FailedToDecodeJson [TooShort, Leftover (List U8)],
    FieldError { path : List Str, error : ResolveFieldError },
]

# TODO: fill in when custom scalars are handled
ResolveFieldError : []

QueryFieldParam data : {
    name : Str,
    typeName : Str,
    encoder : data -> data,
}

QueryFieldInner decodeData output params : {
    name : Str,
    params : List (QueryFieldParam params),
    resolver : decodeData -> Result output ResolveFieldError,
}

QueryField decodeData output params owner := QueryFieldInner decodeData output params

newField = @QueryField

QueryObjectFieldInner decodeData childData innerChild fields params : {
    name : Str,
    params : List (QueryFieldParam params),
    resolver : decodeData -> Result childData ResolveFieldError,
    applyParserToItems : childData, (innerChild -> Result innerChild GraphQLParseError) -> Result childData GraphQLParseError,
    fields : fields,
}

QueryObjectField decodeData childData innerChild fields params owner := QueryObjectFieldInner decodeData childData innerChild fields params

newObjectField = @QueryObjectField

buildQueryStr : { queryName : Str, fields : List QuerySelectorField } -> Str
buildQueryStr = \{ queryName, fields: queryFields } ->
    paramsToStr = \paramsToRender ->
        if List.isEmpty paramsToRender then
            ""
        else
            renderedParams =
                paramsToRender
                |> List.map \{ name, alias } -> "$(name): \$$(alias)"
                |> Str.joinWith ", "

            "($(renderedParams))"

    fieldToStr = \fieldToRender ->
        when fieldToRender is
            Simple { name, params } ->
                "$(name)$(paramsToStr params)"

            Object { name, params, children } ->
                childrenStr =
                    children
                    |> List.map fieldToStr
                    |> Str.joinWith " "
                "$(name)$(paramsToStr params) { $(childrenStr) }"

    collectParams = \fieldWithParams ->
        when fieldWithParams is
            Simple { params } -> params
            Object { params, children } ->
                List.concat params (List.joinMap children collectParams)

    allParams =
        queryFields
        |> List.joinMap collectParams

    paramDefinitions =
        if List.isEmpty allParams then
            ""
        else
            paramsStr =
                allParams
                |> List.map \{ alias, typeName } -> "\$$(alias): $(typeName)"
                |> Str.joinWith ", "

            "($(paramsStr))"

    "query $(queryName)$(paramDefinitions) { $(queryFields |> List.map fieldToStr |> Str.joinWith " ") }"

QuerySelectorBuilder decodeData data params encodeParams owner := {
    fields : List QuerySelectorField,
    # TODO: make List U8
    paramSelector : params -> encodeParams,
    parser : decodeData -> Result data GraphQLParseError,
}

start : state -> QuerySelectorBuilder decodeData state encodeParams encodeParams owner
start = \parser ->
    @QuerySelectorBuilder {
        fields: [],
        paramSelector: \params -> params,
        parser: \_data -> Ok parser,
    }

finish : QuerySelectorBuilder decodeData decodeData params encodeParams owner, Str -> QuerySelector decodeData params where decodeData implements Decoding, encodeParams implements Encoding
finish = \@QuerySelectorBuilder { fields, paramSelector, parser }, queryName ->
    newParser = \jsonBytes ->
        decodedData : Result { data : decodeData } GraphQLParseError
        decodedData =
            Decode.fromBytes jsonBytes Json.utf8
            |> Result.mapErr FailedToDecodeJson

        decodedData
        |> Result.try \{ data } -> parser data

    encodeParams = \paramsData ->
        paramsData
        |> paramSelector
        |> Encode.toBytes Json.utf8

    {
        queryStr: buildQueryStr { fields, queryName },
        encodeParams,
        parser: newParser,
    }

field :
    QueryField decodeData fieldData encodeParams owner
    -> (QuerySelectorBuilder decodeData (fieldData -> state) params encodeParams owner -> QuerySelectorBuilder decodeData state params encodeParams owner)
field = \@QueryField fieldConfig ->
    newParams =
        fieldConfig.params
        |> List.map \param ->
            { name: param.name, alias: param.name, typeName: param.typeName }

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
    QuerySelectorBuilder innerChild innerChild params encodeParams subOwner,
    QueryObjectField decodeData childData innerChild fields encodeParams subOwner
    -> (QuerySelectorBuilder decodeData (childData -> state) params encodeParams owner -> QuerySelectorBuilder decodeData state params encodeParams owner)
object = \objectFieldParserBuilder, @QueryObjectField objectField ->
    newParams =
        objectField.params
        |> List.map \param ->
            { name: param.name, alias: param.name, typeName: param.typeName }

    (@QuerySelectorBuilder objectBuilder) = objectFieldParserBuilder

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
            objectData <- objectDecodeData
                |> objectField.applyParserToItems objectBuilder.parser
                |> Result.try

            Ok (nextParserStage objectData)

        @QuerySelectorBuilder {
            parser: nextParser,
            paramSelector: nextParamSelector,
            fields: fields
            |> List.append
                (
                    Object {
                        name: objectField.name,
                        params: newParams,
                        children: objectBuilder.fields,
                    }
                ),
        }
