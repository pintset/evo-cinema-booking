namespace Common

open Feliz

[<AutoOpen>]
module View =
    let childrenTag f (classes: string list) (children: ReactElement list) =
        f [
            prop.classes classes
            prop.children children
        ]

    let textTag f (classes: string list) (text: string) =
        f [
            prop.classes classes
            prop.text text
        ]


    let asText (classes: string list) (text: string) f =
        f [
            prop.classes classes
            prop.text text
        ]

    let asChildren (classes: string list) (children: ReactElement list) f =
        f [
            prop.classes classes
            prop.children children
        ]

    let div = childrenTag Html.div
    let figure = childrenTag Html.figure
    let a = childrenTag Html.a
    let p = textTag Html.p
    let h1 = textTag Html.h1
    let table = childrenTag Html.table
    let tr = childrenTag Html.tr
    let td = textTag Html.td
    let th = textTag Html.th

    let columns (classes: string list) (children: ReactElement list) =
        div (B.columns :: classes) children

    let column (classes: string list) (children: ReactElement list) =
        div (B.column :: classes) children

module Control =
    let section title =
        div [ B.box; B.mt5; B.isShadowless ] [
            p [ B.title; B.is5; B.hasTextCentered ] title
        ]

    let navBar =
        Html.nav [
            prop.classes [ B.navbar; B.container; B.card; B.isFixedTop ]
            prop.role [| "navigation" |]
            prop.children [
                div [ B.navbarBrand ] [
                    Html.a [
                        prop.classes [ B.navbarItem ]
                        prop.children [ h1 [ B.title ] "MY CINEMA" ]
                        prop.href "#"
                    ]

                    // INSERT BURGER HERE
                ]

                div [ B.navbarMenu ] [
                    div [ B.navbarEnd ] [
                        div [ B.navbarItem ] [
                            div [ B.buttons ] [
                                Html.a |> asText [ B.button; B.isLight ] "Log In"
                            ]
                        ]
                    ]
                ]
            ]
        ]

    let page title main =
        div [] [
            navBar
            section title
            main
        ]
