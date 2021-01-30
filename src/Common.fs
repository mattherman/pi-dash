module Common

open Feliz

let box content =
    Html.div [
        prop.classes [ "box" ]
        prop.children [ content ]
    ]

let titledBox (title: string) content =
    Html.div [
        prop.classes [ "box" ]
        prop.children [ 
            Html.div [
                prop.classes [ "box-title" ]
                prop.children [
                    Html.text title
                ]
            ]
            content
        ]
    ]