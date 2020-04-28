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

type WidgetSize =
    | Single
    | Double

let widget (size: WidgetSize) (children: Fable.React.ReactElement list) =
    let className = if size = Single then "widget widget-single" else "widget widget-double"
    Html.div [
        prop.className className
        prop.children children
    ]

let row (children: Fable.React.ReactElement list) =
    Html.div [
        prop.className "row"
        prop.children children
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "container"
        prop.children [
            row [
                widget Single [ Html.h1 (state.CurrentTime.ToLongTimeString()) ]
                widget Double [ Html.h1 "EMPTY" ]
            ]
            row [
                widget Double [ Html.h1 "EMPTY" ]
                widget Single [ Html.h1 "EMPTY" ]
            ]
            
        ]
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
