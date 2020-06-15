module WeatherWidget

open Elmish
open Fable.SimpleHttp
open Thoth.Json
open Feliz
open Extensions
open Widget

type TemperatureUnit =
    | Fahrenheit
    | Celsius
    | Kelvin

type WeatherCondition = {
    Id: int;
    Description: string;
}

type Weather = {
    WeatherConditions: WeatherCondition list;
    Temperature: (float * TemperatureUnit);
    FeelsLikeTemperature: (float * TemperatureUnit);
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

let init (config: Configuration.WeatherConfig) =
    let units =
        match config.Units with
        | "fahrenheit" -> Fahrenheit
        | "celsius" -> Celsius
        | _ -> Kelvin
    { CurrentWeather = None
      Units = units
      ApiKey = config.ApiKey
      ZipCode = config.ZipCode
      RefreshDurationInSecs = config.Refresh }, Cmd.ofMsg LoadCurrentWeather

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

let convertTemperature (temp: float) units desiredUnits =
    match units, desiredUnits with
    | Kelvin, Fahrenheit ->
        (9.0/5.0) * (temp - 273.15) + 32.0
    | Kelvin, Celsius ->
        temp - 273.15
    | Fahrenheit, Kelvin ->
        temp + 459.67 * (5.0/9.0)
    | Fahrenheit, Celsius ->
        temp - 32.0 * (5.0/9.0)
    | Celsius, Kelvin ->
        temp + 273.15
    | Celsius, Fahrenheit ->
        temp * (9.0/5.0) + 32.0
    | _, _ -> temp

let unitString units =
    match units with
    | Kelvin -> "K"
    | Fahrenheit -> "F"
    | Celsius -> "C"

let renderTemperature weatherOption desiredUnits =
    match weatherOption with
    | Some weather ->
        let (temp, units) = weather.Temperature
        let convertedTemperature = convertTemperature temp units desiredUnits
        let temperatureText = (sprintf "%.1f%s" convertedTemperature (unitString desiredUnits))
        Html.span [
            Html.text temperatureText
        ]
    | None ->
        Html.span [
            Html.text "--"
        ]

let render (state: State) (dispatch: Msg -> unit) =
    widget Double ["weather-widget"] [
        renderTemperature state.CurrentWeather state.Units
    ]