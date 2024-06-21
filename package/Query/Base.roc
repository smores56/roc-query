module [
    buildQueryStr,
    QuerySelector,
    QuerySelectorField,
    QuerySelectorParam,
    GraphQLParseError,
    ResolveFieldError,
    QueryField,
    QueryObjectField,
    QueryFieldParam,
]

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

GraphQLParseError : [
    FailedToDecodeJson [TooShort, Leftover (List U8)],
    FieldError { path : List Str, error : ResolveFieldError },
]

# TODO: fill in when custom scalars are handled
ResolveFieldError : []

QueryField decodeData output params : {
    name : Str,
    params : List (QueryFieldParam params),
    resolver : decodeData -> Result output ResolveFieldError,
}

QueryObjectField decodeData childData fields params : {
    name : Str,
    params : List (QueryFieldParam params),
    resolver : decodeData -> Result childData ResolveFieldError,
    fields : {} -> fields,
}

QueryFieldParam data : {
    name : Str,
    typeName : Str,
    encoder : data -> data,
}
