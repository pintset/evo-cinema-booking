[<AutoOpen>]
module Types

open System

type ApplicationUser =
    | Anonymous
    // | LoggedIn of Api.Use

type MovieId = MovieId of int

module MovieId =
    let value (MovieId id) = id

type Movie =
    { Id: MovieId
      PosterUrl: string
      Title: string
      Description: string }

type TheatreId = TheatreId of Guid

type Theatre =
    { Id: TheatreId
      Name: string }

type AuditoriumId = AuditoriumId of Guid

type Auditorium =
    { Id: AuditoriumId
      TheatreId: TheatreId
      Name: string }

type SessionId = SessionId of Guid

type ShowId = ShowId of Guid

module ShowId =
    let value (ShowId id) = id

type AuditoriumInfo =
    { ShowId: ShowId
      Name: string
      Time: DateTime }

type TheatreInfo =
    { Name: string
      Auditoriums: AuditoriumInfo list }

type Schedule =
    { Date: DateTime 
      Theatres: TheatreInfo list }

type SeatId = SeatId of Guid

module SeatId =
    let value (SeatId id) = id

type AuditoriumSeat =
    { SeatId: SeatId
      Row: int
      Number: int }

type UserId = UserId of Guid

type ReservedSeat =
    { CustomerId: UserId
      Data: AuditoriumSeat }

type ShowSeatState =
    | Free
    | Unavailable
    | ReservedByOtherClient
    | Reserved

type ShowSeat =
    { Id: SeatId
      ShowId: ShowId
      State: ShowSeatState }

type Seat =
    { Id: SeatId
      Row: int
      Number: int
      State: ShowSeatState }

// Main type
type Show =
    { Id: ShowId
      MovieId: MovieId
      AuditoriumId: AuditoriumId
      DateTime: DateTime }

// Just views/infos - show list converted to different structure
type ShowInfo =
    { Id: ShowId
      Title: string
      PosterUrl: string
      Auditorium: string
      Theatre: string
      DateTime: DateTime
      Seats: Seat list }

type Currency =
    | USD
    | EUR
    | BYN

module Currency =
    let toString = function
        | USD -> "USD"
        | EUR -> "EUR"
        | BYN -> "BYN"

type Ticket =
    { Seat: AuditoriumSeat
      Price: decimal }

type Basket =
    { Tickets: Ticket list
      Currency: Currency }

module Basket =
    let overall basket =
        basket.Tickets |> List.sumBy (fun t -> t.Price)

type CustomServerMsg =
    { Command: string
      Data: Guid option }
