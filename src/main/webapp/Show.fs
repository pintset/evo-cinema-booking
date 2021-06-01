module Show

open System

open Elmish
open Elmish.Bridge
open Feliz
open Feliz.Router
open Shared
open Common

type State =
    { Connected: bool
      SessionId: SessionId option
      Show: ShowInfo option
      Seats: Map<SeatId, Seat>
      Basket: Basket }

type Msg =
    | LoadShowInfo of AsyncOperationStatus<Result<ShowInfo, string>>

    | ReleaseSeat of SeatId
    | ReserveSeat of SeatId

    | UpdateSeat of ShowSeat
    | UpdateSeatBulk of ShowSeat list

    | InitializeState of SessionId

    | OpenSession of ShowId
    | FinishSession

    | CloseSession

    | HiFromServer

let init (showId: ShowId) =
    let basket = { Tickets = []; Currency = USD }

    { Connected = false; SessionId = None; Show = None; Seats = Map.empty; Basket = basket }, Cmd.fromAsync (Api.Show.Info.getById showId |> Async.map (Finished >> LoadShowInfo))

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | LoadShowInfo (Finished (Ok show)) ->
        let seats = show.Seats |> List.map (fun seat -> seat.Id, seat) |> Map.ofList
        let command =
            match state.Connected, state.SessionId with
            | true, None ->Cmd.ofMsg (OpenSession show.Id) // Request session in case there is a connection
            | _ -> Cmd.none

        { state with Show = Some show; Seats = seats }, command

    | HiFromServer ->
        let command =
            match state.Show, state.SessionId with
            | Some show, None -> Cmd.ofMsg (OpenSession show.Id) // Request session in case there is a websocket connection
            | _ -> Cmd.none
        { state with Connected = true }, command

    | OpenSession showId -> state, ServerMsg.StartSession showId |> Cmd.bridgeSend

    | FinishSession ->
        Router.navigate Array.empty
        state, ServerMsg.FinishSession |> Cmd.bridgeSend

    | InitializeState sessionId ->
        { state with SessionId = Some sessionId }, Cmd.none

    | ReserveSeat seatId -> state, ServerMsg.ReserveSeat seatId |> Cmd.bridgeSend
    | ReleaseSeat seatId -> state, ServerMsg.ReleaseSeat seatId |> Cmd.bridgeSend

    | UpdateSeat showSeat ->
        let seat =
            let seat = state.Seats.[showSeat.Id]
            { seat with State = showSeat.State }

        let seats = 
            state.Seats |> Map.add seat.Id seat

        let basket =
            match seat.State with
            | Reserved ->
                let tickets =
                    state.Basket.Tickets @ [ { Seat = { SeatId = seat.Id; Number = seat.Number; Row = seat.Row }; Price = 3m } ]
                { state.Basket with Tickets = tickets }
            | Free ->
                let tickets =
                    state.Basket.Tickets |> List.filter (fun t -> t.Seat.SeatId <> seat.Id)
                { state.Basket with Tickets = tickets }
            | _ -> state.Basket

        { state with Seats = seats; Basket = basket }, Cmd.none

    | UpdateSeatBulk showSeats ->
        let seats = showSeats |> List.map (fun showSeat ->
            let seat = state.Seats.[showSeat.Id]
            { seat with State = showSeat.State })

        let seats = seats |> List.fold (fun state seat -> state |> Map.add seat.Id seat) state.Seats

        { state with Seats = seats }, Cmd.none

    | _ -> state, Cmd.none

let renderMovieHorizontal (showInfo: ShowInfo)  =
    div [ B.card; B.mb5 ] [
        div [ B.cardContent ] [
            div [ B.media ] [
                div [ B.mediaLeft ] [
                    figure [ B.image; B.is96X96; B.is2By3 ] [
                        Html.img [
                            prop.src showInfo.PosterUrl
                            prop.alt "Placeholder image"
                        ]
                    ]
                ]
                div [ B.mediaContent ] [
                    p [ B.title; B.is4 ] showInfo.Title
                    sprintf "%s :: %s :: %A" showInfo.Theatre showInfo.Auditorium showInfo.DateTime |> p []
                ]
            ]
        ]
    ]

let renderSeats dispatch (seats: Seat list) =
    let seatButtons =
        seats |> List.groupBy (fun s -> s.Row) |> List.sortBy (fun (row, _) -> row) |> List.map (fun (row, seats) ->
            columns [ B.isCentered; B.isVcentered ] [
                column [ B.hasTextRight ] [
                    Html.p [ prop.text row ] 
                ]
                column [ B.isNarrow ] [
                    seats |> List.sortBy (fun s -> s.Number) |> List.map (fun seat ->
                        let (classes, isDisabled) =
                            match seat.State with
                            | Free -> [], false
                            | Unavailable -> [ B.isWarning ], true
                            | ReservedByOtherClient -> [ B.isInfo ], true
                            | Reserved -> [ B.isPrimary ], false

                        Html.button [
                            prop.key (seat.Id |> SeatId.value)
                            prop.classes <| [ "button"; "is-small" ] @ classes
                            prop.disabled isDisabled
                            prop.onClick <| fun _ ->
                                match seat.State with
                                | Free -> dispatch (ReserveSeat seat.Id)
                                | Reserved -> dispatch (ReleaseSeat seat.Id)
                                | _ -> ()
                        ]
                    ) |> div [ B.buttons ]
                ]
                column [] [
                    Html.p [ prop.text row ] 
                ]
            ]
        )

    div [ B.card; B.mb5 ] [
        div [ B.cardContent ] [
            columns [ B.isCentered; B.isVcentered] [
                column [ B.isNarrow; B.hasTextCentered ] [
                    Html.b "Screen"
                ]
            ]
            yield! seatButtons
        ]
    ]

let renderTickets dispatch (basket: Basket) =
    let renderTicket (ticket: Ticket) =
        tr [] [
            sprintf "Row: %i / Seat: %i" ticket.Seat.Row ticket.Seat.Number |> td []
            td [ B.hasTextRight ] <| sprintf "%.02f" ticket.Price
            basket.Currency |> Currency.toString |> td []
            Html.td [
                prop.children [
                    Html.a [
                        prop.classes [ B.delete ]
                        prop.onClick <| fun _ -> dispatch (ReleaseSeat ticket.Seat.SeatId)
                    ]
                ]
            ]
        ]

    div [ B.card; B.mb5 ] [
        div [ B.cardContent ] [
            div [ B.media ] [
                div [ B.mediaContent ] [
                    p [ B.title; B.is4 ] "My Tickets"
                ]
            ]
            div [ B.content ] [
                table [ B.table ] [
                    basket.Tickets |> List.map renderTicket |> Html.tbody
                    Html.tfoot [
                        Html.tr [
                            th [] "Overall:"
                            basket |> Basket.overall |> sprintf "%.02f" |> th [ B.hasTextRight; B.isNarrow ]
                            basket.Currency |> Currency.toString |> th [ B.isNarrow ]
                            th [] ""
                        ]
                    ]
                ]
                columns [ B.isCentered ] [
                    column [ B.isNarrow ] [
                        let isDisabled = List.isEmpty basket.Tickets

                        Html.button [
                            prop.classes [ B.button ]
                            prop.disabled isDisabled
                            prop.onClick <| fun _ -> dispatch FinishSession
                            prop.text "Proceed to Checkout"
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderMain dispatch  seats basket info =
    div [ B.mt5; B.mx5 ] [
        div [ B.container ] [
            columns [] [
                column [ B.isTwoThirds ] [
                    renderMovieHorizontal info
                    renderSeats dispatch seats
                ]
                column [] [
                    renderTickets dispatch basket
                ]
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    let seats = state.Seats |> Map.toList |> List.map snd
    state.Show |> Option.map (renderMain dispatch seats state.Basket) |> Option.defaultValue Html.none
    |> Control.page "Seats"
