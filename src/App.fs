module App

open Elmish
open Elmish.React
open Feliz
open System

module Cmd =
    let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
        let delayedCmd (dispatch: 'msg -> unit) : unit =
            let delayedDispatch = async {
                let! msg = operation
                dispatch msg
            }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub delayedCmd
        
type State =
    { CurrentTime: DateTime }

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

let render (state: State) (dispatch: Msg -> unit) = Html.h1 (state.CurrentTime.ToLongTimeString())

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
