package com.example.cinemabooking.domain.show

import Common.{SeatId, SessionId, ShowId}

final case class Show(id: ShowId, seats: Map[SeatId, Show.Seat], sessions: Map[SessionId, Show.Session])

object Show {
  // TODO: SeatId for in Show and SeatId in auditorium (physical chair) - should be different things
  // Physical seat can be replaced (broken), show seat for ordered tickets should stay the same
  // So there should be relation between physical seat and show seat
  final case class SeatInfo(id: SeatId, showId: ShowId)
  final case class ReservedSeat(info: SeatInfo, sessionId: SessionId)

  sealed trait Seat
  object Seat {
    final case class Free(info: SeatInfo) extends Seat
    final case class Unavailable(info: SeatInfo) extends Seat
    final case class Reserved(reservedSeat: ReservedSeat) extends Seat
    final case class Paid(info: SeatInfo) extends Seat
  }

  sealed trait SessionStatus
  object SessionStatus {
    final object Started extends SessionStatus
    final object Finished extends SessionStatus
    final object Expired extends SessionStatus
    final object Aborted extends SessionStatus
    final object Prolonged extends SessionStatus
  }

  final case class Session(id: SessionId, showId: ShowId, status: SessionStatus)
}