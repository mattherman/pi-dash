[<RequireQualifiedAccessAttribute>]
module TimeWidget

open Elmish
open Feliz
open System
open Fable.DateFunctions
open Extensions
open Widget

let now = DateTime
type State = {
    CurrentTime: DateTime;
}

type Msg = | Tick

let init() = { CurrentTime = DateTime.Now }, Cmd.ofMsg Tick

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

let render (state: State) (dispatch: Msg -> unit) =
    let time = state.CurrentTime.Format "hh:mm"
    let half = state.CurrentTime.Format "A"
    widget Single ["time-widget"] [
        Html.span [
            prop.children [
                Html.text time
                Html.span [
                    prop.className "am-pm"
                    prop.children [ Html.text half ]
                ]
            ]
        ]
    ]