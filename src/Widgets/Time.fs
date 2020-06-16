[<RequireQualifiedAccessAttribute>]
module Time

open Elmish
open Feliz
open System
open Fable.DateFunctions
open Extensions
open Widget
open Configuration

let now = DateTime
type State = {
    CurrentTime: DateTime;
    Display24HourTime: bool;
}

type Msg = | Tick

let init (config: TimeConfig) =
    { CurrentTime = DateTime.Now; Display24HourTime = config.Display24HourTime }, Cmd.ofMsg Tick

let update (msg: Msg) (state: State) =
    match msg with
    | Tick ->
        let nextState = { state with CurrentTime = DateTime.Now }

        let step =
            async {
                do! Async.Sleep 1000
                return Tick
            }

        nextState, Cmd.fromAsync step

let render12HourTime (time: DateTime) =
    let formattedTime = time.Format "h:mm"
    let formattedPeriod = time.Format "A"
    Html.span [
        prop.children [
            Html.text formattedTime
            Html.span [
                prop.className "time-period"
                prop.children [ Html.text formattedPeriod ]
            ]
        ]
    ]

let render24HourTime (time: DateTime) =
    let formattedTime = time.Format "H:mm"
    Html.span [
        Html.text formattedTime
    ]

let render (state: State) (dispatch: Msg -> unit) =
    widget Single ["time"] [
        if state.Display24HourTime then
            render24HourTime state.CurrentTime
        else
            render12HourTime state.CurrentTime
    ]