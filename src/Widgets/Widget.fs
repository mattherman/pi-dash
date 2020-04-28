module Widget

open Feliz

type WidgetSize =
    | Single
    | Double

let widget (size: WidgetSize) (classes: string list) (children: Fable.React.ReactElement list) =
    let baseClasses = if size = Single then ["widget"; "widget-single"] else ["widget"; "widget-double"]
    Html.div [
        prop.classes (baseClasses @ classes)
        prop.children children
    ]