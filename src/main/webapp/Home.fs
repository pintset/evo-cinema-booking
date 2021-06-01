module Home

open Elmish
open Feliz
open Feliz.Router

open Common

type State =
    { Movies: Movie list }

type Msg =
    | LoadMovies of AsyncOperationStatus<Result<Movie list, string>>
    | BuyTicket of Movie

let init () =
    { Movies = [] }, Started |> LoadMovies |> Cmd.ofMsg

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    // TODO: Sold out
    match msg with 
    | LoadMovies Started -> state, Cmd.fromAsync (Api.Movie.getAll |> Async.map (Finished >> LoadMovies))
    | LoadMovies (Finished (Ok movies)) -> { state with Movies = movies }, Cmd.none
    | LoadMovies (Finished (Error err)) -> { state with Movies = [ { Id = MovieId -1; PosterUrl = "#"; Title = "Error"; Description = err } ] }, Cmd.none

let renderMovie (movie: Movie) dispatch =
    div [ B.card ] [
        div [ B.cardImage ] [
            figure [ B.image; B.is2By3 ] [
                Html.img [
                    prop.src movie.PosterUrl
                    prop.alt "Placeholder image"
                ]
            ]
        ]
        div [ B.cardContent ] [
            div [ B.media ] [
                div [ B.mediaContent ] [
                    p [ B.title; B.is4; B.hasTextCentered ] movie.Title
                ]
            ]
            div [ B.content; B.hasTextCentered ] [
                Html.a [
                    prop.classes [ B.button; B.isSmall ]
                    prop.href <| Router.format [ "schedule"; movie.Id |> MovieId.value |> string  ]
                    prop.text "Buy Ticket"
                ]
            ]
        ]
    ]

let renderMovies movies dispatch =
    let renderMovie movie =
        column [] [
            renderMovie movie dispatch
        ]

    div [ B.mx5 ] [
        movies |> List.map renderMovie |> List.chunkBySize 4 |> List.map (fun l -> columns [] l) |> div [ B.container ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Control.page "Now Playing" <| renderMovies state.Movies dispatch