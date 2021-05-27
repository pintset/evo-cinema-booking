package com.example.cinemabooking.domain.show

import java.util.UUID

object Common {
  final case class SeatId(value: UUID)
  final case class SessionId(value: UUID)
  final case class ShowId(value: UUID)
}
