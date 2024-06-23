module []

import Query
import Utils

buildQuery : Str, (_ -> Query.QuerySelectorBuilder decodeData decodeData params encodeParams { query : {} }) -> Query.QuerySelector decodeData params where decodeData implements Decoding, encodeParams implements Encoding
buildQuery = \queryName, selectorBuilder ->
    selectorBuilder {
        root: queryObject {},
        user: userObject {},
        address: addressObject {},
    }
    |> Query.finish queryName

queryObject : {}
    -> {
        loggedIn : Query.QueryField { loggedIn : Bool }* Bool {}* { query : {} },
        user : Query.QueryObjectField { user : childData2 }* childData2 childData2 _ { userId : I64 }* { user : {} },
    }
queryObject = \{} -> {
    loggedIn: Query.newField {
        name: "loggedIn",
        params: [],
        resolver: \{ loggedIn } -> Ok loggedIn,
    },
    user: Query.newObjectField {
        name: "user",
        params: [{ name: "userId", typeName: "Int!", encoder: \x -> x }],
        resolver: \{ user } -> Ok user,
        applyParserToItems: Utils.trySingle,
        fields: userObject {},
    },
}

userObject : {}
    -> {
        name : Query.QueryField { name : Str }* Str {}* { user : {} },
        age : Query.QueryField { age : I64 }* I64 {}* { user : {} },
        address : Query.QueryObjectField { address : List childData3 }* (List childData3) childData3 _ { primary : Bool }* { address : {} },
    }
userObject = \{} -> {
    name: Query.newField {
        name: "name",
        params: [],
        resolver: \{ name } -> Ok name,
    },
    age: Query.newField {
        name: "age",
        params: [],
        resolver: \{ age } -> Ok age,
    },
    address: Query.newObjectField {
        name: "address",
        params: [{ name: "primary", typeName: "Bool!", encoder: \x -> x }],
        resolver: \{ address } -> Ok address,
        applyParserToItems: Utils.tryList,
        fields: addressObject {},
    },
}

addressObject : {}
    -> {
        street : Query.QueryField { street : Str }* Str {}* { address : {} },
        state : Query.QueryField { state : Str }* Str {}* { address : {} },
        zipCode : Query.QueryField { zipCode : I64 }* I64 {}* { address : {} },
    }
addressObject = \{} -> {
    street: Query.newField {
        name: "street",
        params: [],
        resolver: \{ street } -> Ok street,
    },
    state: Query.newField {
        name: "state",
        params: [],
        resolver: \{ state } -> Ok state,
    },
    zipCode: Query.newField {
        name: "zipCode",
        params: [],
        resolver: \{ zipCode } -> Ok zipCode,
    },
}

userQueryLarge =
    buildQuery "UserQueryLarge" \{ root, user, address } ->
        Query.start {
            user: <-
                Query.start {
                    name: <- Query.field user.name,
                    age: <- Query.field user.age,
                    address: <-
                        Query.start {
                            state: <- Query.field address.state,
                            zipCode: <- Query.field address.zipCode,
                        }
                        |> Query.object user.address,
                }
                |> Query.object root.user,
        }

userQuerySmall =
    buildQuery "UserQuerySmall" \{ root, user } ->
        Query.start {
            user: <-
                Query.start {
                    name: <- Query.field user.name,
                    age: <- Query.field user.age,
                }
                |> Query.object root.user,
        }

loggedInQuery =
    buildQuery "LoggedInQuery" \{ root } ->
        Query.start {
            loggedIn: <- Query.field root.loggedIn,
        }

expect
    { queryStr } = userQueryLarge
    queryStr == "query UserQueryLarge($userId: Int!, $primary: Bool!) { user(userId: $userId) { name age address(primary: $primary) { state zipCode } } }"

expect
    jsonData = "{\"data\": {\"user\": {\"name\": \"John Doe\", \"age\": 30, \"address\": [{\"state\": \"Georgia\", \"zipCode\": 30305}]}}}"
    parsedData =
        jsonData
        |> Str.toUtf8
        |> userQueryLarge.parser

    parsedData
    == Ok {
        user: {
            name: "John Doe",
            age: 30,
            address: [
                {
                    state: "Georgia",
                    zipCode: 30305,
                },
            ],
        },
    }

expect
    actual =
        userQueryLarge.encodeParams { userId: 3, primary: Bool.true }
        |> Str.fromUtf8
    expected = "{\"primary\":true,\"userId\":3}"

    actual == Ok expected

expect
    { queryStr } = userQuerySmall
    queryStr == "query UserQuerySmall($userId: Int!) { user(userId: $userId) { name age } }"

expect
    jsonData = "{\"data\": {\"user\": {\"name\": \"John Doe\", \"age\": 30}}}"
    parsedData =
        jsonData
        |> Str.toUtf8
        |> userQuerySmall.parser

    parsedData
    == Ok {
        user: {
            name: "John Doe",
            age: 30,
        },
    }

expect
    actual =
        userQuerySmall.encodeParams { userId: 3 }
        |> Str.fromUtf8
    expected = "{\"userId\":3}"

    actual == Ok expected

expect
    { queryStr } = loggedInQuery
    queryStr == "query LoggedInQuery { loggedIn }"

expect
    jsonData = "{\"data\": {\"loggedIn\": true}}"
    parsedData =
        jsonData
        |> Str.toUtf8
        |> loggedInQuery.parser

    parsedData
    == Ok { loggedIn: Bool.true }

expect
    actual =
        loggedInQuery.encodeParams {}
        |> Str.fromUtf8
    expected = "{}"

    actual == Ok expected
