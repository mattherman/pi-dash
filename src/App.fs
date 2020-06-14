[<RequireQualifiedAccessAttribute>]
module App

open Elmish
open Feliz
open Fable.Core
open Extensions
open Configuration

type State = {
    Time: TimeWidget.State;
}

type Msg = | TimeMsg of TimeWidget.Msg

let initializeWithConfig (config: Config) =
    fun () ->
        let timeWidgetState, timeWidgetCmd = TimeWidget.init config.TimeConfig
        let initialState = {
            Time = timeWidgetState
        }
        let initialCmd = Cmd.batch [
            Cmd.map TimeMsg timeWidgetCmd
        ]
        initialState, initialCmd

let update (msg: Msg) (state: State) =
    match msg with
    | TimeMsg timeMsg ->
        let updatedTimeWidgetState, timeWidgetCmd = TimeWidget.update timeMsg state.Time
        { state with Time = updatedTimeWidgetState }, Cmd.map TimeMsg timeWidgetCmd

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
                TimeWidget.render state.Time (TimeMsg >> dispatch)
                EmptyWidget.render
                EmptyWidget.render
            ]
            row [
                EmptyWidget.render
                EmptyWidget.render
                EmptyWidget.render
            ]
        ]
    ]