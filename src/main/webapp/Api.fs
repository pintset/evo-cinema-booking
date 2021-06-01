module Api

open FsToolkit.ErrorHandling

open Fable.SimpleHttp
open Thoth.Json

open Endpoint

let getData decoder url =
    url |> Http.get |> Async.map (function
        | 200, bodyString -> Decode.fromString decoder bodyString
        | code, _ -> "Http error while loading movie data: " + string code |> Error)

module Movie =
    let decoder = Decode.object <| fun get ->
        let id = get.Required.Field "id" Decode.int
        { Id = MovieId id
          PosterUrl = httpApi "/movies" + sprintf "/%i/poster" id
          Title = get.Required.Field "title" Decode.string
          Description = get.Required.Field "description" Decode.string }

    let getAll =
        httpApi "/movies" |> getData (Decode.list decoder)

    let getById (MovieId id) =
        httpApi "/movies/" + string id |> getData decoder

module Theatre =
    let decoder = Decode.object <| fun get ->
        { Id = get.Required.Field "id" Decode.guid |> TheatreId
          Name = get.Required.Field "name" Decode.string }

    let getAll =
        httpApi "/theatres" |> getData (Decode.list decoder)

    let getById (TheatreId id) =
        httpApi "/theatres/" + string id |> getData decoder

module Auditorium =
    let decoder = Decode.object <| fun get ->
        { Id = get.Required.Field "id" Decode.guid |> AuditoriumId
          TheatreId = get.Required.Field "theatreId" Decode.guid |> TheatreId
          Name = get.Required.Field "name" Decode.string }

    let getAll =
        httpApi "/auditoriums" |> getData (Decode.list decoder)

    let getById (AuditoriumId id) =
        httpApi "/auditoriums/" + string id |> getData decoder

module Show =
    let decoder = Decode.object <| fun get ->
        { Id = get.Required.Field "id" Decode.guid |> ShowId
          MovieId = get.Required.Field "movieId" Decode.int |> MovieId
          AuditoriumId = get.Required.Field "auditoriumId" Decode.guid |> AuditoriumId
          DateTime = get.Required.Field "dateTime" Decode.datetime }

    let getAll =
        httpApi "/shows" |> getData (Decode.list decoder)

    module Info =
        let getById (ShowId showId) =
            let seatDecoder = Decode.object <| fun get ->
                let state = get.Required.Field "state" Decode.string

                let showSeatState =
                    match state with
                    | "free" -> Free
                    | "unavailable" -> Unavailable
                    | "reserved" -> ReservedByOtherClient // Bad design (should be different 3 opts DU)
                    | state -> failwithf "Failed to decode seat state: %s" state

                { Id = get.Required.Field "seatId" Decode.guid |> SeatId
                  Row = get.Required.Field "row" Decode.int
                  Number = get.Required.Field "number" Decode.int
                  State = showSeatState }

            let decoder = Decode.object <| fun get ->
                { Id = get.Required.Field "showId" Decode.guid |> ShowId
                  Title = get.Required.Field "title" Decode.string
                  PosterUrl = get.Required.Field "posterUrl" Decode.string
                  Auditorium = get.Required.Field "auditorium" Decode.string
                  Theatre = get.Required.Field "theatre" Decode.string
                  DateTime = get.Required.Field "dateTime" Decode.datetime
                  Seats = get.Required.Field "seats" (Decode.list seatDecoder) }

            httpApi "/shows/" + string showId + "/info" |> getData decoder

module Schedule =
    let getById (MovieId id) =
        let auditoriumInfoDecoder = Decode.object <| fun get ->
            { ShowId = get.Required.Field "showId" Decode.guid |> ShowId
              Name = get.Required.Field "name" Decode.string
              Time = get.Required.Field "time" Decode.datetime }

        let theatreInfoDecoder = Decode.object <| fun get ->
            { Name = get.Required.Field "name" Decode.string
              Auditoriums = get.Required.Field "auditoriums" (Decode.list auditoriumInfoDecoder) }

        let scheduleDecoder = Decode.object <| fun get ->
            { Date = get.Required.Field "date" Decode.datetime
              Theatres = get.Required.Field "theatres" (Decode.list theatreInfoDecoder) }

        httpApi "/movies/" + string id + "/schedule" |> getData (Decode.list scheduleDecoder)


module CustomServerMsg =
    open Elmish.Bridge

    let createWithData command id =
        { Command = command; Data = Some id }

    let create command =
        { Command = command; Data = None }

    let inline encode x = Encode.Auto.toString(0, x, CamelCase) |> Text

    let serializer = (function
        | Shared.ServerMsg.StartSession showId -> showId |> ShowId.value |> createWithData "StartSession"
        | Shared.ServerMsg.FinishSession -> create "FinishSession"
        | Shared.ServerMsg.AbortSession -> create "AbortSession"
        | Shared.ServerMsg.ReserveSeat seatId -> seatId |> SeatId.value |> createWithData "ReserveSeat"
        | Shared.ServerMsg.ReleaseSeat seatId -> seatId |> SeatId.value |> createWithData "ReleaseSeat") >> encode
