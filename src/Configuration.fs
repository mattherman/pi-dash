module Configuration

open Thoth.Json

type TimeConfig = {
    Display24HourTime: bool;
}

type WeatherConfig = {
    Units: string;
    ApiKey: string;
    Refresh: int;
    ZipCode: int;
}

type Config = {
    TimeConfig: TimeConfig
    WeatherConfig: WeatherConfig
}

let timeConfigDecoder : Decoder<TimeConfig> =
    Decode.object (fun get -> {
        Display24HourTime = get.Optional.At [ "display24HourTime" ] Decode.bool |> Option.defaultValue false
    })

let weatherConfigDecoder : Decoder<WeatherConfig> =
    Decode.object (fun get -> {
        Units = get.Optional.At [ "units" ] Decode.string |> Option.defaultValue "standard"
        ApiKey = get.Required.At [ "apiKey" ] Decode.string
        Refresh = get.Optional.At [ "refreshRateInSecs" ] Decode.int |> Option.defaultValue 600
        ZipCode = get.Required.At [ "location" ] Decode.int
    })

let configDecoder : Decoder<Config> =
    Decode.object (fun get -> {
        TimeConfig = get.Required.At [ "time" ] timeConfigDecoder
        WeatherConfig = get.Required.At [ "weather" ] weatherConfigDecoder
    })