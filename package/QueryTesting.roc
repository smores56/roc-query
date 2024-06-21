module []

import Query.Builder as QB
import Query.Base exposing [
    QueryObjectField,
    QueryField,
]

queryLoggedIn : {} -> QueryField _ Bool _
queryLoggedIn = \{} -> {
    name: "loggedIn",
    params: [],
    resolver: \{ loggedIn } -> Ok loggedIn,
}

queryUser : {} -> QueryObjectField _ _ _ { userId : I64 }a
queryUser = \{} -> {
    name: "user",
    params: [{ name: "userId", typeName: "Int!", encoder: \x -> x }],
    resolver: \{ user } -> Ok user,
    fields: testUserObject,
}

testUserObject : {}
    -> {
        name : {} -> QueryField _ Str _,
        age : {} -> QueryField _ I64 _,
        address : {} -> QueryObjectField _ _ _ { primary : Bool }a,
    }
testUserObject = \{} -> {
    name: \{} -> {
        name: "name",
        params: [],
        resolver: \{ name } -> Ok name,
    },
    age: \{} -> {
        name: "age",
        params: [],
        resolver: \{ age } -> Ok age,
    },
    address: \{} -> {
        name: "address",
        params: [
            { name: "primary", typeName: "Bool!", encoder: \x -> x },
        ],
        resolver: \{ address } -> Ok address,
        fields: testAddressObject,
    },
}

testAddressObject : {}
    -> {
        street : {} -> QueryField _ Str _,
        state : {} -> QueryField _ Str _,
        zipCode : {} -> QueryField _ I64 _,
    }
testAddressObject = \{} -> {
    street: \{} -> {
        name: "street",
        params: [],
        resolver: \{ street } -> Ok street,
    },
    state: \{} -> {
        name: "state",
        params: [],
        resolver: \{ state } -> Ok state,
    },
    zipCode: \{} -> {
        name: "zipCode",
        params: [],
        resolver: \{ zipCode } -> Ok zipCode,
    },
}

userQueryLarge =
    QB.start {
        user: <-
            QB.object queryUser \user ->
                QB.start {
                    name: <- QB.field user.name,
                    age: <- QB.field user.age,
                    address: <-
                        QB.objectWithRenamedParams
                            user.address
                            (\{ primary, userId } -> { userId, isPrimary: primary })
                            (Dict.single "primary" "isPrimary")
                            \addr ->
                                QB.start {
                                    state: <- QB.field addr.state,
                                    zipCode: <- QB.field addr.zipCode,
                                },
                },
    }
    |> QB.finish "UserQueryLarge"

userQuerySmall =
    QB.start {
        user: <-
            QB.object queryUser \user ->
                QB.start {
                    name: <- QB.field user.name,
                    age: <- QB.field user.age,
                },
    }
    |> QB.finish "UserQuerySmall"

loggedInQuery =
    QB.start {
        loggedIn: <- QB.field queryLoggedIn,
    }
    |> QB.finish "LoggedInQuery"

expect
    { queryStr } = userQueryLarge
    queryStr == "query UserQueryLarge($userId: Int!, $isPrimary: Bool!) { user(userId: $userId) { name age address(primary: $isPrimary) { state zipCode } } }"

expect
    jsonData = "{\"data\": {\"user\": {\"name\": \"John Doe\", \"age\": 30, \"address\": {\"state\": \"Georgia\", \"zipCode\": 30305}}}}"
    parsedData =
        jsonData
        |> Str.toUtf8
        |> userQueryLarge.parser

    parsedData
    == Ok {
        user: {
            name: "John Doe",
            age: 30,
            address: {
                state: "Georgia",
                zipCode: 30305,
            },
        },
    }

expect
    actual =
        userQueryLarge.encodeParams { userId: 3, isPrimary: Bool.true }
        |> Str.fromUtf8
    expected = "{\"isPrimary\":true,\"userId\":3}"

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
