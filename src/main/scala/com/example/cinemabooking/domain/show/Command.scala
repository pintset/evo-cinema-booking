package com.example.cinemabooking.domain.show

import Common._

sealed trait Command
object Command {
  final case class StartSession (sessionId: SessionId) extends Command
  final case class FinishSession(sessionId: SessionId) extends Command
  final case class AbortSession (sessionId: SessionId) extends Command

  final case class ReserveSeat(sessionId: SessionId, seatId: SeatId) extends Command
  final case class ReleaseSeat(sessionId: SessionId, seatId: SeatId) extends Command
}
