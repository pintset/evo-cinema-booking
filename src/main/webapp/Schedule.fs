module Schedule

open System

open Elmish
open Feliz
open Feliz.Router

open FsToolkit.ErrorHandling

open Common

type State =
    { Movie: Movie option
      Schedule: Schedule list
      CurrentDate: DateTime option }

type Msg =
    | LoadMovieAndSchedule of AsyncOperationStatus<Result<(Movie option * Schedule list), string>>
    | ChangeDate of DateTime

type Data =
    | Movie of Movie
    | Schedule of Schedule list

let getData id = async {
    let! data =
        [ Api.Movie.getById id |> AsyncResult.map Movie; Api.Schedule.getById id |> AsyncResult.map Schedule ]
        |> Async.Parallel

    let movie =
        match data.[0] with
        | Ok (Movie movie) -> Some movie
        | _ -> None

    let schedule =
        match data.[1] with
        | Ok (Schedule schedule) -> schedule
        | _ -> []

    return Ok (movie, schedule)
}

let init (movieId: MovieId) =
    { Movie = None; Schedule = []; CurrentDate = None }, Cmd.fromAsync (getData movieId |> Async.map (Finished >> LoadMovieAndSchedule))

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | LoadMovieAndSchedule (Finished (Ok (movie, schedule))) ->
        { Movie = movie
          Schedule = schedule
          CurrentDate = schedule |> List.map (fun s -> s.Date) |> List.sort |> List.tryHead }
        , Cmd.none
    | ChangeDate date -> { state with CurrentDate = Some date }, Cmd.none

    // TODO: Comment this to check other cases (error ones)
    | _ -> state, Cmd.none

let renderMovieVertical (movie: Movie) =
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
            Html.div [
                prop.classes [ B.content ]
                prop.text movie.Description
            ]
        ]
    ]

let renderDates dispatch (currentDate: DateTime option) (dates: DateTime list) =
    div [ B.card; B.mb5 ] [
        div [ B.cardContent ] [
            dates |> List.sort |> List.map (fun date ->
                let text = 
                    if date.Day = DateTime.Today.Day then "Today"
                    else if date.Day = DateTime.Today.AddDays(1.).Day then "Tomorrow"
                    else Date.Format.localFormat Date.Local.englishUS "dddd dd.MM" date

                Html.button [
                    prop.classes [ B.button; if currentDate.IsSome && currentDate.Value.Day = date.Day then B.isFocused ]
                    prop.text text
                    prop.onClick <| fun _ -> dispatch (ChangeDate date)
                ]
            ) |> div [ B.buttons ]
        ]
    ]

let renderSchedule theatres =
    div [ B.card; B.mb5 ] [ 
        theatres |> List.map (fun theatre ->
            columns [ B.isVcentered ] [
                column [ B.is3; B.mx1 ] [
                    Html.b [ prop.text theatre.Name ] 
                ]
                column [ B.mx1 ] [
                    let btns =
                        theatre.Auditoriums
                        |> List.sortBy (fun a -> a.Time)
                        |> List.map (fun a ->
                            let time = Date.Format.localFormat Date.Local.englishUS "HH:mm" (a.Time.ToLocalTime())
                            let text = sprintf "%s: %s" a.Name time
                            Html.a [
                                prop.classes [ B.button ]
                                prop.text text
                                prop.href <| Router.format [ "show"; a.ShowId |> ShowId.value |> string ]
                            ])
                    div [ B.buttons ] btns
                ]
            ])
        |> div [ B.cardContent ]
    ]

let renderMain state dispatch =
    div [ B.mt5; B.mx5 ] [
        div [ B.container ] [
            columns [] [
                column [ B.isThreeQuarters ] [
                    state.Schedule |> List.map (fun d -> d.Date) |> renderDates dispatch state.CurrentDate

                    if state.CurrentDate.IsSome then
                        let dayView = state.Schedule |> List.find (fun dayView -> dayView.Date = state.CurrentDate.Value)
                        renderSchedule dayView.Theatres
                ]
                column [] [
                    state.Movie |> Option.map renderMovieVertical |> Option.defaultValue Html.none
                ]
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Control.page "Theatres" <| renderMain state dispatch
