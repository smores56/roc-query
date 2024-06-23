app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    weaver: "https://github.com/smores56/weaver/releases/download/0.2.0/BBDPvzgGrYp-AhIDw0qmwxT0pWZIQP_7KOrUrZfp_xw.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.0/KbIfTNbxShRX1A1FgXei1SpO5Jn8sgP6HP6PXbi-xyA.tar.br",
    rq: "../package/main.roc",
}

import cli.Task exposing [Task]
import cli.Arg
import cli.File
import cli.Http
import cli.Stdout
import cli.Path
import weaver.Cli
import weaver.Param
import weaver.Subcommand
import rq.Codegen
import rq.Parse
import rq.Query
import Client

dataQuery =
    Client.buildQuery "DataQuery" \{ query, book } ->
        Query.start {
            books: <-
                Query.start {
                    author: <- Query.field book.author,
                }
                |> Query.object query.books,
        }

queryParams = {}

main =
    args = Arg.list!
    mainTask =
        when Cli.parseOrDisplayMessage argParser args is
            Err message ->
                Stdout.line! message
                Task.ok {}

            Ok { subcommand } ->
                when subcommand is
                    Codegen { path, modulePath } ->
                        generateGraphQLModule {
                            path: Path.fromStr path,
                            modulePath: Path.fromStr modulePath,
                        }

                    Fetch {} ->
                        fetchDataFromApi

    Task.onErr mainTask \err ->
        message =
            when err is
                x -> Inspect.toStr x

        Stdout.line message

generateGraphQLModule = \{ path, modulePath } ->
    schemaFile =
        File.readUtf8 path
            |> Task.mapErr! FailedToReadSchemaFile
            |> Str.split "\n"
    schema =
        schemaFile
            |> Parse.parseSchema
            |> Task.fromResult!
    moduleCode = Codegen.generateModule { schema }
    File.writeUtf8! modulePath moduleCode

fetchDataFromApi =
    apiUrl = "http://localhost:8000"
    requestBody = dataQuery.encodeParams queryParams
    request = { Http.defaultRequest & body: requestBody, url: apiUrl }
    response =
        Http.send request
            |> Task.mapErr! \HttpError err -> SomeErr (Http.errorToString err)
    data =
        response.body
            |> dataQuery.parser
            |> Result.mapErr \err -> SomeErr (Inspect.toStr err)
            |> Task.fromResult!
    Stdout.line! "Got data back from API:"
    Stdout.line! (Inspect.toStr data)

argParser =
    Cli.weave {
        subcommand: <- Subcommand.required [codegenSubcommand, fetchSubcommand],
    }
    |> Cli.finish {
        name: "roc-query-example",
        description: "An example of using the `roc-query` library.",
    }
    |> Cli.assertValid

codegenSubcommand =
    Cli.weave {
        path: <- Param.str { name: "path", help: "The path to the " },
        modulePath: <- Param.str { name: "module-path", help: "The path of the module to generate" },
    }
    |> Subcommand.finish {
        name: "codegen",
        description: "Generate a .roc module with the client code for the provided schema.",
        mapper: Codegen,
    }

fetchSubcommand =
    Cli.weave {}
    |> Subcommand.finish {
        name: "fetch",
        description: "Fetch data using a query defined using the generated schema client.",
        mapper: Fetch,
    }

