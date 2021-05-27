package com.example.cinemabooking.domain.show

import Common.SessionId
import Show._

sealed trait ReservationError
object ReservationError {
  final case class SessionNotFound(sessionId: SessionId) extends ReservationError
  final case class InactiveSession(session: Session)     extends ReservationError
  final case class SeatNotFound(info: SeatInfo)          extends ReservationError
  final case class SeatCannotBeReserved(seat: Seat)      extends ReservationError
}

sealed trait ReleaseError
object ReleaseError {
  final case class SessionNotFound(sessionId: SessionId)  extends ReleaseError
  final case class InactiveSession(session: Session)      extends ReleaseError
  final case class SeatNotFound(info: SeatInfo)           extends ReleaseError
  final case class SeatCannotBeReleased(seat: Seat)       extends ReleaseError
}

sealed trait SessionError

sealed trait Error
object Error {
  final case class Session(error: SessionError)         extends Error
  final case class Reservation(error: ReservationError) extends Error
  final case class Release(error: ReleaseError)         extends Error
}