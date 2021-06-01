module App

open Elmish
open Elmish.Bridge
open Elmish.React

open Feliz
open Feliz.Router

open Common

[<RequireQualifiedAccess>]
type Url =
    | Home
    | Schedule of MovieId
    | Show of ShowId
    | NotFound

module Url =
    let parse = function
        | [] -> Url.Home
        | [ "schedule"; Route.Int movieId ] -> movieId |> MovieId |> Url.Schedule 
        | [ "show"; Route.Guid showId ] -> showId |> ShowId |> Url.Show
        | _ -> Url.NotFound

type Msg =
    | UrlChanged of Url
    | HomeMsg of Home.Msg
    | ScheduleMsg of Schedule.Msg
    | ShowMsg of Show.Msg

[<RequireQualifiedAccess>]
type Page =
    | Home of Home.State
    | Schedule of Schedule.State
    | Show of Show.State * BridgeConfig<Msg, Msg>
    | NotFound

type State =
    { User: ApplicationUser
      CurrentUrl: Url
      CurrentPage: Page }

let config (): BridgeConfig<Msg, Msg> =
    Bridge.endpoint Endpoint.socket
    // |> Bridge.withUrlMode Raw
    // |> Bridge.withWhenDown ClientMsg.ConnectionLost
    |> Bridge.withRetryTime 5
    |> Bridge.withCustomSerializer Api.CustomServerMsg.serializer

let pageCommand url =
    match url with
    | Url.Home ->
        let state, cmd = Home.init ()
        Page.Home state, cmd |> Cmd.map HomeMsg
    | Url.Schedule id ->
        let state, cmd = Schedule.init id
        Page.Schedule state, cmd |> Cmd.map ScheduleMsg
    | Url.Show showId ->
        let state, cmd = Show.init showId
        let config = config ()
        Page.Show (state, config), Cmd.batch [
            config |> Bridge.asSubscription |> Cmd.ofSub
            cmd |> Cmd.map ShowMsg
        ]
    | Url.NotFound ->
        Page.NotFound, Cmd.none

let init () =
    let url = Router.currentUrl () |> Url.parse

    let page, command = pageCommand url

    let state =
        { User = Anonymous
          CurrentUrl = url
          CurrentPage = page }

    state, command

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let wrap msgType pageType stateCommand =
        let subState, subCmd = stateCommand
        let nextState = { state with CurrentPage = pageType subState }
        let nextCmd = subCmd |> Cmd.map msgType
        nextState, nextCmd

    match state.CurrentPage, msg with    
    | Page.Home homeState, HomeMsg homeMsg ->
        Home.update homeMsg homeState |> wrap HomeMsg Page.Home

    | Page.Schedule scheduleState, ScheduleMsg scheduleMsg ->
        Schedule.update scheduleMsg scheduleState |> wrap ScheduleMsg Page.Schedule

    | Page.Show (showState, config), ShowMsg showMsg ->
        Show.update showMsg showState |> wrap ShowMsg (fun state -> Page.Show (state, config))

    | _, UrlChanged nextUrl ->
        match state.CurrentUrl, state.CurrentPage with
        | Url.Show _, Page.Show (_, config) ->
            (config :> System.IDisposable).Dispose ()
        | _ -> ()

        let nextPage, nextCommand = pageCommand nextUrl
        { state with CurrentPage = nextPage; CurrentUrl = nextUrl }, nextCommand

    | _ -> state, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    let activePage =
        match state.CurrentPage with
        | Page.Home homeState -> Home.render homeState (HomeMsg >> dispatch)
        | Page.Schedule scheduleState -> Schedule.render scheduleState (ScheduleMsg >> dispatch)
        | Page.Show (showState, _) -> Show.render showState (ShowMsg >> dispatch)
        | Page.NotFound -> p [] "Not found"

    React.router [
        router.onUrlChanged (Url.parse >> UrlChanged >> dispatch)
        router.children [ activePage ]
    ]

Program.mkProgram init update render
// |> Program.withBridgeConfig config
|> Program.withConsoleTrace
// |> Program.withReactBatched "elmish-app"
|> Program.withReactSynchronous "elmish-app"
|> Program.run
