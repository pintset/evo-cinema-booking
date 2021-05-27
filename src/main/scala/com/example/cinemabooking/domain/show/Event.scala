package com.example.cinemabooking.domain.show

import Common.{SessionId, ShowId}

sealed trait Event
object Event {
  final case class SessionStarted (sessionId: SessionId, showId: ShowId) extends Event
  final case class SessionFinished(sessionId: SessionId)                 extends Event
  final case class SessionAborted (sessionId: SessionId)                 extends Event

  final case class SeatReserved(seat: Show.ReservedSeat) extends Event
  final case class SeatReleased(info: Show.SeatInfo)     extends Event
}
