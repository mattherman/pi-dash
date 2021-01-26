[<RequireQualifiedAccessAttribute>]
module Time

open Elmish
open Feliz
open System
open Fable.DateFunctions
open Extensions
open Configuration

type State = {
    Now: DateTime;
    Display24HourTime: bool;
}

type Msg = | Tick

let init (config: TimeConfig) =
    { Now = DateTime.Now; Display24HourTime = config.Display24HourTime }, Cmd.ofMsg Tick

let update (msg: Msg) (state: State) =
    match msg with
    | Tick ->
        let nextState = { state with Now = DateTime.Now }

        let step =
            async {
                do! Async.Sleep 1000
                return Tick
            }

        nextState, Cmd.fromAsync step

let render12HourTime (date: DateTime) =
    let formattedTime = date.Format "h:mm"
    let formattedPeriod = date.Format "A"
    Html.div [
        prop.children [
            Html.text formattedTime
            Html.span [
                prop.classes [ "time-period" ]
                prop.children [ Html.text formattedPeriod ]
            ]
        ]
    ]

let render24HourTime (date: DateTime) =
    let formattedTime = date.Format "H:mm"
    Html.div [
        Html.text formattedTime
    ]

let renderDate (date: DateTime) =
    let formattedDate = date.Format "MMMM do, YYYY"
    Html.div [
        prop.classes [ "time-formatted-date" ]
        prop.children [
            Html.text formattedDate
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [ "box"; "time" ]
        prop.children [
            if state.Display24HourTime then
                render24HourTime state.Now
            else
                render12HourTime state.Now
            renderDate state.Now
        ]
    ]