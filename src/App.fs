[<RequireQualifiedAccessAttribute>]
module App

open Elmish
open Feliz
open Extensions

[<RequireQualifiedAccessAttribute>]
type WidgetType =
    | Time
    | Empty

[<RequireQualifiedAccessAttribute>]
type WidgetState =
    | Time of TimeWidget.State
    | Empty

type Widget = {
    Name: string;
    Type: WidgetType;
    State: WidgetState;
}

type State = {
    Widgets: Map<string, Widget>
}

type WidgetMsg =
    | Time of string * TimeWidget.Msg

type Msg = | WidgetMsg of WidgetMsg

let initWidget name widgetType =
    let (state, cmd) = 
        match widgetType with
        | WidgetType.Time ->
            let timeWidgetState, timeWidgetCmd = TimeWidget.init(name);
            WidgetState.Time timeWidgetState, Cmd.map Time timeWidgetCmd
        | WidgetType.Empty ->
            WidgetState.Empty, Cmd.none
    { Name = name; Type = widgetType; State = state }, cmd

let init() =
    let widgets = [
        initWidget "time" WidgetType.Time
        initWidget "empty_1" WidgetType.Empty
        initWidget "empty_2" WidgetType.Empty
        initWidget "empty_3" WidgetType.Empty
        initWidget "empty_4" WidgetType.Empty
        initWidget "empty_5" WidgetType.Empty
    ]

    let widgetByName widgetAndCmd =
        let widget = fst widgetAndCmd
        (widget.Name, widget)

    let initialState = {
        Widgets = List.map widgetByName widgets |> Map.ofList
    }
    let initialCmd = Cmd.batch (List.map snd widgets)
    initialState, initialCmd

let updateWidget (msg: WidgetMsg) (state: State) =
    match msg with
    | Time (name, timeMsg) ->
        let widgetToUpdate = Map.find name state.Widgets
        let (newState, cmd) = 
            match widgetToUpdate.State with
            | WidgetState.Time widgetState ->
                let updatedTimeWidgetState, timeWidgetCmd = TimeWidget.update timeMsg widgetState
                let updatedWidget = { widgetToUpdate with State = (WidgetState.Time updatedTimeWidgetState) }
                let updatedWidgetMap =
                    state.Widgets
                    |> Map.remove name
                    |> Map.add name updatedWidget
                { state with Widgets = updatedWidgetMap }, Cmd.map Time timeWidgetCmd
            | _ -> state, Cmd.none
        newState, cmd

let update (msg: Msg) (state: State) =
    match msg with
    | WidgetMsg widgetMsg -> updateWidget widgetMsg state

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
                TimeWidget.render state.Time (Time >> dispatch)
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

let render2 (state: State) (dispatch: Msg -> unit) =
    state.Widgets
    |> Map.toList
    |> List.map (fun (_, widget) -> 
        match widget.Type with
            | WidgetType.Time ->
                TimeWidget.render widget.State (Time >> dispatch)
            | _ ->
                EmptyWidget.render
        )
        