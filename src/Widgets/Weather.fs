module Weather

open Elmish
open Fable.SimpleHttp
open Thoth.Json
open Feliz
open Extensions
open Widget
open Configuration

type TemperatureUnit =
    | Fahrenheit
    | Celsius
    | Kelvin

type Temperature = float * TemperatureUnit

type WeatherCondition = {
    Id: int;
    Description: string;
}

type Weather = {
    WeatherConditions: WeatherCondition list;
    Temperature: Temperature;
    FeelsLikeTemperature: Temperature
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
        Temperature = (get.Required.At [ "main"; "temp" ] Decode.float, Kelvin)
        FeelsLikeTemperature = (get.Required.At [ "main"; "feels_like" ] Decode.float, Kelvin)
        Humidity = get.Required.At [ "main"; "humidity" ] Decode.int
        WindSpeed = get.Required.At [ "wind"; "speed" ] Decode.float
        WindDirection = get.Required.At [ "wind"; "deg" ] Decode.int
        CityName = get.Required.At [ "name" ] Decode.string
    })

type State = {
    Units: TemperatureUnit
    ApiKey: string
    ZipCode: int;
    RefreshDurationInSecs: int;
    CurrentWeather: Weather option
}

type Msg =
    | LoadCurrentWeather
    | LoadedCurrentWeather of Result<Weather, string>

let toTemperatureUnits units =
    match units with
    | "fahrenheit" -> Fahrenheit
    | "celsius" -> Celsius
    | _ -> Kelvin

let init (config: WeatherConfig) =
    let initialState =
        { CurrentWeather = None
          Units = config.Units |> toTemperatureUnits
          ApiKey = config.ApiKey
          ZipCode = config.ZipCode
          RefreshDurationInSecs = config.Refresh }
    initialState, Cmd.ofMsg LoadCurrentWeather

let loadCurrentTemperature apiKey zipCode =
    async {
        let endpoint = sprintf "https://api.openweathermap.org/data/2.5/weather?zip=%d&appid=%s" zipCode apiKey
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
        state, Cmd.fromAsync (loadCurrentTemperature state.ApiKey state.ZipCode)
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

let convertTemperature desiredUnits (temperatureWithUnits: Temperature) =
    let (temperatureValue, temperatureUnits) = temperatureWithUnits
    match temperatureUnits, desiredUnits with
    | Kelvin, Fahrenheit ->
        (9.0/5.0) * (temperatureValue - 273.15) + 32.0
    | Kelvin, Celsius ->
        temperatureValue - 273.15
    | Fahrenheit, Kelvin ->
        temperatureValue + 459.67 * (5.0/9.0)
    | Fahrenheit, Celsius ->
        temperatureValue - 32.0 * (5.0/9.0)
    | Celsius, Kelvin ->
        temperatureValue + 273.15
    | Celsius, Fahrenheit ->
        temperatureValue * (9.0/5.0) + 32.0
    | _, _ -> temperatureValue

let unitString units =
    match units with
    | Kelvin -> "K"
    | Fahrenheit -> "F"
    | Celsius -> "C"

let renderTemperature weatherOption desiredUnits =
    match weatherOption with
    | Some weather ->
        let convertedTemperature = weather.Temperature |> convertTemperature desiredUnits
        let temperatureText = sprintf "%.1f" convertedTemperature
        let unitText = sprintf "°%s" (unitString desiredUnits)
        Html.div [
            prop.children [
                Html.text temperatureText
                Html.span [
                    prop.className "unit"
                    prop.children [ Html.text unitText ]
                ]
            ]
        ]
    | None ->
        Html.span [
            Html.text "--"
        ]

let render (state: State) (dispatch: Msg -> unit) =
    widget Double ["weather-widget"] [
        renderTemperature state.CurrentWeather state.Units
    ]