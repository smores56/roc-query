module [buildQuery]

import json.Option exposing [Option]
import rq.Query
import rq.Utils

buildQuery : Str, (_ -> Query.QuerySelectorBuilder decodeData decodeData params encodeParams { query : {} }) -> Query.QuerySelector decodeData params where decodeData implements Decoding, encodeParams implements Encoding
buildQuery = \queryName, selectorBuilder ->
    selectorBuilder {
        book: bookObject {},
        query: queryObject {},
        mutation: mutationObject {},
    }
    |> Query.finish queryName

bookObject : {}
    -> {
        id : Query.QueryField { id : Str }* Str {}a { bookObject: {} },
        name : Query.QueryField { name : Str }* Str {}a { bookObject: {} },
        author : Query.QueryField { author : Str }* Str {}a { bookObject: {} },
    }
bookObject = \{} -> {
    id: Query.newField {
        name: "id",
        params: [],
        resolver: \{ id } -> Ok id,
    },
    name: Query.newField {
        name: "name",
        params: [],
        resolver: \{ name } -> Ok name,
    },
    author: Query.newField {
        name: "author",
        params: [],
        resolver: \{ author } -> Ok author,
    },
}

queryObject : {}
    -> {
        books : Query.QueryObjectField { books : (List (Option childData0)) }* ((List (Option childData0))) childData0 _ {}a { bookObject: {} },
    }
queryObject = \{} -> {
    books: Query.newObjectField {
        name: "books",
        params: [],
        resolver: \{ books } -> Ok books,
        applyParserToItems: Utils.tryListOfOptions,
        fields: bookObject {},
    },
}

mutationObject : {}
    -> {
        createBook : Query.QueryField { createBook : Str }* Str { name : Str, author : Str }* { mutationObject: {} },
        deleteBook : Query.QueryField { deleteBook : F64 }* F64 { id : Str }* { mutationObject: {} },
    }
mutationObject = \{} -> {
    createBook: Query.newField {
        name: "createBook",
        params: [
            { name: "name", typeName: "String!", encoder: \x -> x },
            { name: "author", typeName: "String!", encoder: \x -> x },
        ],
        resolver: \{ createBook } -> Ok createBook,
    },
    deleteBook: Query.newField {
        name: "deleteBook",
        params: [
            { name: "id", typeName: "ID!", encoder: \x -> x },
        ],
        resolver: \{ deleteBook } -> Ok deleteBook,
    },
}
