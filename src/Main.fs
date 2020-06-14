module Main

open Elmish
open Elmish.React
open Thoth.Json
open Fable.SimpleHttp
open Configuration

let loadConfigAndRun =
    async {
        let! (statusCode, configJson) = Http.get "config.json"
        if statusCode = 200 then
            let configParseResult = Decode.fromString configDecoder configJson
            match configParseResult with
            | Ok config ->
               Program.mkProgram (App.initializeWithConfig config) App.update App.render
               |> Program.withReactSynchronous "elmish-app"
               |> Program.run 
            | Error err -> printfn "Failed to parse configuration: %s" err
        else
            printfn "Failed to load configuration from file 'config.json'"
    }

Async.StartImmediate loadConfigAndRun