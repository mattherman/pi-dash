[<RequireQualifiedAccessAttribute>]
module App

open Elmish
open Feliz
open Fable.Core
open Extensions
open Configuration

type State = {
    Time: Time.State;
    Weather: Weather.State;
}

type Msg =
    | TimeMsg of Time.Msg
    | WeatherMsg of Weather.Msg

let initializeWithConfig (config: Config) =
    fun () ->
        let timeState, timeCmd = Time.init config.TimeConfig
        let weatherState, weatherCmd = Weather.init config.WeatherConfig
        let initialState = {
            Time = timeState
            Weather = weatherState
        }
        let initialCmd = Cmd.batch [
            Cmd.map TimeMsg timeCmd
            Cmd.map WeatherMsg weatherCmd
        ]
        initialState, initialCmd

let update (msg: Msg) (state: State) =
    match msg with
    | TimeMsg timeMsg ->
        let updatedTimeState, timeCmd = Time.update timeMsg state.Time
        { state with Time = updatedTimeState }, Cmd.map TimeMsg timeCmd
    | WeatherMsg weatherMsg ->
        let updatedWeatherState, weatherCmd = Weather.update weatherMsg state.Weather
        { state with Weather = updatedWeatherState }, Cmd.map WeatherMsg weatherCmd

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [ "container" ]
        prop.children [
            Time.render state.Time (TimeMsg >> dispatch)
            Weather.render state.Weather (WeatherMsg >> dispatch)
        ]
    ]