package com.example.cinemabooking.domain.show

import cats.implicits._

import com.example.cinemabooking.Data

import Common._
import Show._

object Projection {
  def init(showId: ShowId, auditoriumSeats: List[Data.Auditorium.Seat]): Show = {
    val seats: Map[SeatId, Seat] =
      auditoriumSeats.map { seat =>
        val seatId = SeatId(seat.seatId)
        val seatInfo = SeatInfo(seatId, showId)

        // TODO: Remove this alg from here
        val showSeat = {
          def isEven(n: Int): Boolean = n % 2 == 0

          def isDividableBy3(n: Int): Boolean = n % 3 == 0

          if ((!isEven(seat.row) && !isEven(seat.number)) || (isEven(seat.row) && isDividableBy3(seat.number)))
            Seat.Unavailable(seatInfo)
          else
            Seat.Free(seatInfo)
        }
        (seatId, showSeat)
      }.toMap

    Show(showId, seats, Map.empty)
  }

  def update(show: Show, event: Event): Show =
    event match {
      case Event.SessionStarted(sessionId, showId) =>
        val session = Session(sessionId, showId, SessionStatus.Started)
        val newState = show.copy(sessions = show.sessions + (sessionId -> session))
        newState

      case Event.SessionFinished(sessionId) =>
        val seats = show.seats.values.toList.mapFilter {
          case Seat.Reserved(seat) if seat.sessionId == sessionId => Some(seat)
          case _ => None
        }.foldLeft(show.seats) { (state, seat) =>
          state + (seat.info.id -> Seat.Paid(seat.info))
        }
        show.copy(sessions = show.sessions - sessionId, seats = seats)

      case Event.SessionAborted(sessionId) =>
        val seats = show.seats.values.toList.mapFilter {
          case Seat.Reserved(seat) if seat.sessionId == sessionId => Some(seat)
          case _ => None
        }.foldLeft(show.seats) { (state, seat) =>
          state + (seat.info.id -> Seat.Free(seat.info))
        }
        show.copy(sessions = show.sessions - sessionId, seats = seats)

      case Event.SeatReserved(seat) =>
        show.copy(seats = show.seats + (seat.info.id -> Seat.Reserved(seat)))

      case Event.SeatReleased(seat) =>
        show.copy(seats = show.seats + (seat.id -> Seat.Free(seat)))
    }
}
