[<RequireQualifiedAccessAttribute>]
module App

open Elmish
open Feliz
open Fable.Core
open Extensions
open Configuration

type State = {
    Time: TimeWidget.State;
    Weather: WeatherWidget.State;
}

type Msg =
    | TimeMsg of TimeWidget.Msg
    | WeatherMsg of WeatherWidget.Msg

let initializeWithConfig (config: Config) =
    fun () ->
        let timeWidgetState, timeWidgetCmd = TimeWidget.init config.TimeConfig
        let weatherWidgetState, weatherWidgetCmd = WeatherWidget.init config.WeatherConfig
        let initialState = {
            Time = timeWidgetState
            Weather = weatherWidgetState
        }
        let initialCmd = Cmd.batch [
            Cmd.map TimeMsg timeWidgetCmd
            Cmd.map WeatherMsg weatherWidgetCmd
        ]
        initialState, initialCmd

let update (msg: Msg) (state: State) =
    match msg with
    | TimeMsg timeMsg ->
        let updatedTimeWidgetState, timeWidgetCmd = TimeWidget.update timeMsg state.Time
        { state with Time = updatedTimeWidgetState }, Cmd.map TimeMsg timeWidgetCmd
    | WeatherMsg weatherMsg ->
        let updatedWeatherWidgetState, weatherWidgetCmd = WeatherWidget.update weatherMsg state.Weather
        { state with Weather = updatedWeatherWidgetState }, Cmd.map WeatherMsg weatherWidgetCmd

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
                WeatherWidget.render state.Weather (WeatherMsg >> dispatch)
                EmptyWidget.render
            ]
            row [
                EmptyWidget.render
                EmptyWidget.render
                EmptyWidget.render
            ]
        ]
    ]