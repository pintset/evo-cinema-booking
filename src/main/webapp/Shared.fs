module Shared

type ServerMsg =
    | StartSession of ShowId
    | FinishSession

    | ReserveSeat of SeatId
    | ReleaseSeat of SeatId

    | AbortSession