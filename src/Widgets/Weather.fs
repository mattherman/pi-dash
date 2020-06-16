module Weather

open Elmish
open Fable.SimpleHttp
open Thoth.Json
open Feliz
open Extensions
open Widget
open Configuration

type MeasurementSystem =
    | Standard
    | Imperial
    | Metric

type WeatherCondition = {
    Id: int;
    Description: string;
}

type Weather = {
    WeatherConditions: WeatherCondition list;
    Temperature: float;
    FeelsLikeTemperature: float;
    Humidity: int;
    WindSpeed: float;
    WindDirection: int;
    CityName: string;
}

let weatherConditionDecoder : Decoder<WeatherCondition> =
    Decode.object (fun get -> {
        Id = get.Required.At [ "id" ] Decode.int
        Description = get.Required.At [ "main" ] Decode.string
    })

let weatherDecoder : Decoder<Weather> =
    Decode.object (fun get -> {
        WeatherConditions = get.Required.At [ "weather" ] (Decode.list weatherConditionDecoder)
        Temperature = (get.Required.At [ "main"; "temp" ] Decode.float)
        FeelsLikeTemperature = (get.Required.At [ "main"; "feels_like" ] Decode.float)
        Humidity = get.Required.At [ "main"; "humidity" ] Decode.int
        WindSpeed = get.Required.At [ "wind"; "speed" ] Decode.float
        WindDirection = get.Required.At [ "wind"; "deg" ] Decode.int
        CityName = get.Required.At [ "name" ] Decode.string
    })

type State = {
    Units: MeasurementSystem
    ApiKey: string
    ZipCode: int;
    RefreshDurationInSecs: int;
    CurrentWeather: Weather option
}

type Msg =
    | LoadCurrentWeather
    | LoadedCurrentWeather of Result<Weather, string>

let toMeasurementSystem units =
    match units with
    | "imperial" -> Imperial
    | "metric" -> Metric
    | _ -> Standard

let toUnits measurementSystem =
    match measurementSystem with
    | Imperial -> "imperial"
    | Metric -> "metric"
    | Standard -> "standard"

let init (config: WeatherConfig) =
    let initialState =
        { CurrentWeather = None
          Units = config.Units |> toMeasurementSystem
          ApiKey = config.ApiKey
          ZipCode = config.ZipCode
          RefreshDurationInSecs = config.Refresh }
    initialState, Cmd.ofMsg LoadCurrentWeather

let loadCurrentTemperature apiKey zipCode units =
    async {
        let unitsParameter =
            match units with
            | Imperial -> "&units=imperial"
            | Metric -> "&units=metric"
            | Standard -> ""
        let endpoint = sprintf "https://api.openweathermap.org/data/2.5/weather?zip=%d&appid=%s%s" zipCode apiKey unitsParameter
        let! (status, responseContent) = Http.get endpoint
        match status with
        | 200 ->
            let parsedResult = Decode.fromString weatherDecoder responseContent
            return LoadedCurrentWeather parsedResult
        | _ ->
            return LoadedCurrentWeather (Error responseContent)
    }

let update (msg: Msg) (state: State) =
    match msg with
    | LoadCurrentWeather ->
        state, Cmd.fromAsync (loadCurrentTemperature state.ApiKey state.ZipCode state.Units)
    | LoadedCurrentWeather result ->
        match result with
        | Ok weather ->
            let refreshWeather = async {
                do! Async.Sleep (state.RefreshDurationInSecs * 1000)
                return LoadCurrentWeather
            }
            { state with CurrentWeather = Some weather }, Cmd.fromAsync refreshWeather
        | Error err ->
            { state with CurrentWeather = None }, Cmd.none

let temperatureUnits units =
    match units with
    | Standard -> "K"
    | Imperial -> "F"
    | Metric -> "C"

let renderTemperature weather units =
    let temperatureText = sprintf "%.1f" weather.Temperature
    let feelsLikeTemperatureText = sprintf "Feels like %.0f°" weather.FeelsLikeTemperature
    let unitText = sprintf "°%s" (temperatureUnits units)
    Html.div [
        prop.children [
            Html.div [
                prop.className "temperature"
                prop.children [
                    Html.text temperatureText
                    Html.span [
                        prop.className "unit"
                        prop.children [ Html.text unitText ]
                    ]
                ]
                
            ]
            Html.div [
                prop.className "feels-like-temperature"
                prop.children [
                    Html.text feelsLikeTemperatureText
                ]
            ]
        ]
    ]

let renderWeatherCondition (weatherCondition: WeatherCondition) =
    Html.span [
        prop.className "condition"
        prop.children [
            Html.div [
                Html.i [
                    prop.classes [ "condition-icon"; "wi"; "wi-day-sunny" ]
                ]
            ]
            Html.div [
                Html.text weatherCondition.Description
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    match state.CurrentWeather with
    | Some weather ->
        widget Double ["weather-widget"] [
            renderTemperature weather state.Units
            if not (List.isEmpty weather.WeatherConditions) then
                renderWeatherCondition weather.WeatherConditions.Head
        ]
    | None ->
        widget Double ["weather-widget"] [
            Html.text "--"
        ]