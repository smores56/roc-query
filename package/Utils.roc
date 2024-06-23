module [
    trySingle,
    tryList,
    tryListList,
    tryOption,
    tryOptionList,
    tryListOfOptions,
]

import json.Option exposing [Option]

trySingle : a, (a -> Result b err) -> Result b err
trySingle = \item, tryToParse ->
    tryToParse item

tryList : List a, (a -> Result b err) -> Result (List b) err
tryList = \items, tryToParse ->
    List.mapTry items tryToParse

tryListList : List (List a), (a -> Result b err) -> Result (List (List b)) err
tryListList = \nestedList, tryToParse ->
    nestedList
    |> List.mapTry \list ->
        list |> List.mapTry tryToParse

tryOption : Option a, (a -> Result b err) -> Result (Option b) err
tryOption = \option, tryToParse ->
    when Option.get option is
        Some data -> tryToParse data |> Result.map Option.some
        None -> Ok (Option.none {})

tryOptionList : Option (List a), (a -> Result b err) -> Result (Option (List b)) err
tryOptionList = \optionList, tryToParse ->
    when Option.get optionList is
        Some data -> List.mapTry data tryToParse |> Result.map Option.some
        None -> Ok (Option.none {})

tryListOfOptions : List (Option a), (a -> Result b err) -> Result (List (Option b)) err
tryListOfOptions = \listOfOptions, tryToParse ->
    listOfOptions
    |> List.mapTry \option ->
        tryOption option tryToParse
