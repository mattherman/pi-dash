module Configuration

open Thoth.Json

type TimeConfig = {
    Display24HourTime: bool;
}

type Config = {
    TimeConfig: TimeConfig
}

let timeConfigDecoder : Decoder<TimeConfig> =
    Decode.object (fun get -> {
        Display24HourTime = get.Required.At [ "display24HourTime" ] Decode.bool
    })

let configDecoder : Decoder<Config> =
    Decode.object (fun get -> {
        TimeConfig = get.Required.At [ "time" ] timeConfigDecoder
    })