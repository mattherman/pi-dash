module Weather

open Elmish
open Fable.SimpleHttp
open Thoth.Json
open Feliz
open Extensions
open Widget
open Configuration
open System

type GeographicLocation = {
    Latitude: float;
    Longitude: float;
}

let geographicLocationDecoder : Decoder<GeographicLocation> =
    Decode.object (fun get -> {
        Latitude = get.Required.At [ "coord"; "lat" ] Decode.float
        Longitude = get.Required.At [ "coord"; "lon" ] Decode.float
    })

type MeasurementSystem =
    | Standard
    | Imperial
    | Metric

type WeatherCondition = {
    Id: int;
    Description: string;
}

type CurrentWeather = {
    WeatherConditions: WeatherCondition list;
    Temperature: float;
    FeelsLikeTemperature: float;
    WindSpeed: float;
    WindDirection: int;
}

type DailyWeather = {
    Date: DateTime;
    HighTemperature: float;
    LowTemperature: float;
    WeatherConditions: WeatherCondition list;
    Rain: float option;
}

type Weather = {
    Current: CurrentWeather;
    Daily: DailyWeather list;
}

let weatherConditionDecoder : Decoder<WeatherCondition> =
    Decode.object (fun get -> {
        Id = get.Required.At [ "id" ] Decode.int
        Description = get.Required.At [ "main" ] Decode.string
    })

let currentWeatherDecoder : Decoder<CurrentWeather> =
    Decode.object (fun get -> {
        WeatherConditions = get.Required.At [ "weather" ] (Decode.list weatherConditionDecoder)
        Temperature = get.Required.At [ "temp" ] Decode.float
        FeelsLikeTemperature = get.Required.At [ "feels_like" ] Decode.float
        WindSpeed = get.Required.At [ "wind_speed" ] Decode.float
        WindDirection = get.Required.At [ "wind_deg" ] Decode.int
    })

let unixEpochToTimestamp unix =
    let epoch = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    epoch.AddMilliseconds unix

let dailyWeatherDecoder : Decoder<DailyWeather> =
    Decode.object (fun get -> {
        Date = get.Required.At [ "dt" ] Decode.float |> unixEpochToTimestamp
        HighTemperature = get.Required.At [ "temp"; "max" ] Decode.float
        LowTemperature = get.Required.At [ "temp"; "min" ] Decode.float
        WeatherConditions = get.Required.At [ "weather" ] (Decode.list weatherConditionDecoder)
        Rain = get.Optional.At [ "rain" ] Decode.float
    })

let weatherDecoder : Decoder<Weather> =
    Decode.object (fun get -> {
        Current = get.Required.At [ "current" ] currentWeatherDecoder
        Daily = get.Required.At [ "daily" ] (Decode.list dailyWeatherDecoder)
    })

type State = {
    Units: MeasurementSystem
    ApiKey: string
    ZipCode: int;
    RefreshDurationInSecs: int;
    Location: GeographicLocation option;
    Weather: Weather option;
}

type Msg =
    | LoadLocation
    | LoadedLocation of Result<GeographicLocation, string>
    | LoadWeather
    | LoadedWeather of Result<Weather, string>

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
        { Location = None
          Weather = None
          Units = config.Units |> toMeasurementSystem
          ApiKey = config.ApiKey
          ZipCode = config.ZipCode
          RefreshDurationInSecs = config.Refresh }
    initialState, Cmd.ofMsg LoadLocation

let loadLocation apiKey zipCode = async {
    // We need the latitude/longitude of the zip in order to call the endpoint that
    // will return current weather + forecasted weather. Instead of introducing a separate
    // geocoding API, we are going to use the current weather API which will take a zip and returns
    // geographic location on its result. We will then use this to look up the complete weather separately.
    let endpoint = sprintf "https://api.openweathermap.org/data/2.5/weather?zip=%d&appid=%s" zipCode apiKey
    let! (status, responseContent) = Http.get endpoint
    match status with
    | 200 ->
        let parsedResult = Decode.fromString geographicLocationDecoder responseContent
        return LoadedLocation parsedResult
    | _ ->
        return LoadedLocation (Error responseContent)
}

let loadWeather apiKey location units = async {
    let lat = location.Latitude
    let lon = location.Longitude
    let unitsParameter =
        match units with
        | Imperial -> "&units=imperial"
        | Metric -> "&units=metric"
        | Standard -> ""
    let exclude = "minutely,hourly"
    let endpoint = sprintf "https://api.openweathermap.org/data/2.5/onecall?lat=%f&lon=%f&exclude=%s&appid=%s%s" lat lon exclude apiKey unitsParameter
    let! (status, responseContent) = Http.get endpoint
    match status with
    | 200 ->
        let parsedResult = Decode.fromString weatherDecoder responseContent
        return LoadedWeather parsedResult
    | _ ->
        return LoadedWeather (Error responseContent)
}

let update (msg: Msg) (state: State) =
    match msg with
    | LoadLocation ->
        state, Cmd.fromAsync (loadLocation state.ApiKey state.ZipCode)
    | LoadedLocation result ->
        match result with
        | Ok location ->
            { state with Location = Some location }, Cmd.ofMsg LoadWeather
        | Error err ->
            printfn "[ERROR] Failed to load location: %s" err
            { state with Location = None }, Cmd.none
    | LoadWeather ->
        match state.Location with
        | Some location ->
            state, Cmd.fromAsync (loadWeather state.ApiKey location state.Units)
        | None ->
            state, Cmd.none
    | LoadedWeather result ->
        match result with
        | Ok weather ->
            let refreshWeather = async {
                do! Async.Sleep (state.RefreshDurationInSecs * 1000)
                return LoadWeather
            }
            { state with Weather = Some weather }, Cmd.fromAsync refreshWeather
        | Error err ->
            printfn "[ERROR] Failed to load weather: %s" err
            { state with Weather = None }, Cmd.none

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
        prop.className "temperature"
        prop.children [
            Html.div [
                prop.className "current-temperature"
                prop.children [
                    Html.text temperatureText
                    Html.span [
                        prop.className "temperature-unit"
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

let renderTemperatureRange weather =
    Html.div [
        prop.className "temperature-range"
        prop.children [
            Html.div [
                Html.text (sprintf "%.0f°" weather.HighTemperature)
                Html.i [
                    prop.classes [ "wi"; "wi-direction-up" ]
                ]
            ]
            Html.div [
                Html.text (sprintf "%.0f°" weather.LowTemperature)
                Html.i [
                    prop.classes [ "wi"; "wi-direction-down" ]
                ]
            ]
        ]
    ]

let renderWeatherCondition (weatherCondition: WeatherCondition) =
    let iconClass = sprintf "wi-owm-%d" weatherCondition.Id
    Html.div [
        prop.className "weather-condition"
        prop.children [
            Html.div [
                Html.i [
                    prop.classes [ "weather-condition-icon"; "wi"; iconClass ]
                ]
            ]
            Html.div [
                Html.text weatherCondition.Description
            ]
        ]
    ]

let windSpeedUnits units =
    match units with
    | Standard | Metric -> "m/s"
    | Imperial -> "mph"

let renderWind weather units =
    let iconClass = sprintf "towards-%d-deg" weather.WindDirection
    Html.div [
        prop.className "wind"
        prop.children [
            Html.div [
                Html.i [
                    prop.classes [ "wind-icon"; "wi"; "wi-wind"; iconClass ]
                ]
            ]
            Html.div [
                Html.text (sprintf "%.0f %s" weather.WindSpeed (windSpeedUnits units))
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    match state.Weather with
    | Some weather ->
        widget Widget.Double ["weather"] [
            Html.div [
                prop.className "current-weather"
                prop.children [
                    Html.div [
                        prop.className "temperature-container"
                        prop.children [
                            renderTemperature weather.Current state.Units
                            if not (List.isEmpty weather.Daily) then
                                renderTemperatureRange (weather.Daily |> List.head)
                        ]
                    ]
                    Html.div [
                        prop.className "weather-condition-container"
                        prop.children [
                            if not (List.isEmpty weather.Current.WeatherConditions) then
                                renderWeatherCondition weather.Current.WeatherConditions.Head
                            renderWind weather.Current state.Units
                        ]
                    ]
                ]
            ]
            Html.div [
                prop.className "forecasted-weather"
                prop.children [
                    Html.text "FORECAST"
                ]
            ]
        ]
    | None ->
        widget Widget.Double [ "no-weather" ] [
            Html.span [
                Html.text "--"
            ]
        ]