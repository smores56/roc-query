module [
    removePrefix,
    parseLowerCamelCaseWord,
    parseUpperCamelCaseWord,
    parseScreamingSnakeCaseWord,
    takeCharsWhile,
    dropCharsWhile,
    isLowerCaseLetter,
    isUpperCaseLetter,
    isLetter,
    parseDigit,
    parseByte,
    isNonZeroDigit,
    isDigit,
    isWhitespace,
    InvalidLowerCamelCaseWord,
    InvalidUpperCamelCaseWord,
    InvalidScreamingSnakeCaseWord,
]

InvalidLowerCamelCaseWord : [
    Empty,
    FirstLetterMustBeLowerCase,
]

InvalidUpperCamelCaseWord : [
    Empty,
    FirstLetterMustBeUpperCase,
]

InvalidScreamingSnakeCaseWord : [
    Empty,
    FirstLetterMustBeUpperCase,
    DoubleUnderscoreNotAllowed,
    CannotEndWithUnderscore,
]

removePrefix : Str, Str -> Result Str [PrefixNotPresent]
removePrefix = \text, prefix ->
    when Str.splitFirst text prefix is
        Ok { before: "", after } -> Ok after
        _otherwise -> Err PrefixNotPresent

parseLowerCamelCaseWord : Str -> Result (Str, Str) InvalidLowerCamelCaseWord
parseLowerCamelCaseWord = \text ->
    chars = Str.toUtf8 text

    walkRes =
        List.walkUntil chars (Ok (GetFirstChar, 0)) \stateRes, char ->
            when stateRes is
                Err err -> Break (Err err)
                Ok (action, charCount) ->
                    when action is
                        GetFirstChar ->
                            if isLowerCaseLetter char then
                                Continue (Ok (GetLetter, charCount + 1))
                            else
                                Break (Err FirstLetterMustBeLowerCase)

                        GetLetter ->
                            if isLowerCaseLetter char || isUpperCaseLetter char then
                                Continue (Ok (GetLetter, charCount + 1))
                            else
                                Break (Ok (GetLetter, charCount))

    when walkRes is
        Err err -> Err err
        Ok (finalAction, charCount) ->
            when finalAction is
                GetFirstChar -> Err Empty
                GetLetter ->
                    if charCount == 0 then
                        Err Empty
                    else
                        Ok (
                            chars
                            |> List.takeFirst charCount
                            |> Str.fromUtf8
                            |> Result.withDefault "",
                            chars
                            |> List.dropFirst charCount
                            |> Str.fromUtf8
                            |> Result.withDefault "",
                        )

parseUpperCamelCaseWord : Str -> Result (Str, Str) InvalidUpperCamelCaseWord
parseUpperCamelCaseWord = \text ->
    chars = Str.toUtf8 text

    walkRes =
        List.walkUntil chars (Ok (GetFirstChar, 0)) \stateRes, char ->
            when stateRes is
                Err err -> Break (Err err)
                Ok (action, charCount) ->
                    when action is
                        GetFirstChar ->
                            if isUpperCaseLetter char then
                                Continue (Ok (GetLetter, charCount + 1))
                            else
                                Break (Err FirstLetterMustBeUpperCase)

                        GetLetter ->
                            if isLowerCaseLetter char || isUpperCaseLetter char then
                                Continue (Ok (GetLetter, charCount + 1))
                            else
                                Break (Ok (GetLetter, charCount))

    when walkRes is
        Err err -> Err err
        Ok (finalAction, charCount) ->
            when finalAction is
                GetFirstChar -> Err Empty
                GetLetter ->
                    if charCount == 0 then
                        Err Empty
                    else
                        Ok (
                            chars
                            |> List.takeFirst charCount
                            |> Str.fromUtf8
                            |> Result.withDefault "",
                            chars
                            |> List.dropFirst charCount
                            |> Str.fromUtf8
                            |> Result.withDefault "",
                        )

parseScreamingSnakeCaseWord : Str -> Result (Str, Str) InvalidScreamingSnakeCaseWord
parseScreamingSnakeCaseWord = \text ->
    chars = Str.toUtf8 text

    walkRes =
        List.walkUntil chars (Ok (GetFirstChar, 0)) \stateRes, char ->
            when stateRes is
                Err err -> Break (Err err)
                Ok (action, charCount) ->
                    when action is
                        GetFirstChar ->
                            if isUpperCaseLetter char then
                                Continue (Ok (GetLetterOrUnderscore, charCount + 1))
                            else
                                Break (Err FirstLetterMustBeUpperCase)

                        GetLetterOrUnderscore ->
                            if isUpperCaseLetter char then
                                Continue (Ok (GetLetterOrUnderscore, charCount + 1))
                            else if isUnderscore char then
                                Continue (Ok (GetLetter, charCount + 1))
                            else
                                Break (Ok (GetLetterOrUnderscore, charCount))

                        GetLetter ->
                            if isUpperCaseLetter char then
                                Continue (Ok (GetLetterOrUnderscore, charCount + 1))
                            else if isUnderscore char then
                                Break (Err DoubleUnderscoreNotAllowed)
                            else
                                Break (Ok (GetLetterOrUnderscore, charCount))

    when walkRes is
        Err err -> Err err
        Ok (finalAction, charCount) ->
            when finalAction is
                GetFirstChar -> Err Empty
                GetLetter -> Err CannotEndWithUnderscore
                GetLetterOrUnderscore ->
                    if charCount == 0 then
                        Err Empty
                    else
                        Ok (
                            chars
                            |> List.takeFirst charCount
                            |> Str.fromUtf8
                            |> Result.withDefault "",
                            chars
                            |> List.dropFirst charCount
                            |> Str.fromUtf8
                            |> Result.withDefault "",
                        )

isLetter = \char ->
    isLowerCaseLetter char || isUpperCaseLetter char

isLowerCaseLetter = \char ->
    char >= 'a' && char <= 'z'

isUpperCaseLetter = \char ->
    char >= 'A' && char <= 'Z'

isUnderscore = \char ->
    char == '_'

parseByte : List U8, U8 -> Result (List U8) [ByteNotFound]
parseByte = \chars, byte ->
    when chars is
        [first, .. as rest] if first == byte -> Ok rest
        _notFound -> Err ByteNotFound

parseDigit : U8 -> Result U64 [InvalidDigit]
parseDigit = \asciiDigit ->
    if isDigit asciiDigit then
        Ok (Num.toU64 (asciiDigit - '0'))
    else
        Err InvalidDigit

isDigit = \char ->
    char >= '0' && char <= '9'

isNonZeroDigit = \char ->
    char >= '1' && char <= '9'

isWhitespace = \char ->
    when char is
        ' ' | '\n' | '\t' | '\r' -> Bool.true
        _ -> Bool.false

takeCharsWhile : List U8, (U8 -> Bool) -> (List U8, List U8)
takeCharsWhile = \chars, shouldTakeChar ->
    List.walkUntil chars ([], chars) \(taken, untaken), char ->
        if shouldTakeChar char then
            Continue (List.append taken char, List.dropFirst untaken 1)
        else
            Break (taken, untaken)

dropCharsWhile : List U8, (U8 -> Bool) -> List U8
dropCharsWhile = \chars, shouldDropChar ->
    numberOfCharsToDrop =
        List.walkUntil chars 0 \droppedSoFar, char ->
            if shouldDropChar char then
                Continue (droppedSoFar + 1)
            else
                Break droppedSoFar

    chars
    |> List.dropFirst numberOfCharsToDrop
