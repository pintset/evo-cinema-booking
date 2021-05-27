package com.example.cinemabooking.server

import com.example.cinemabooking.domain.show.Common._

import java.util.UUID

sealed trait ShowSeatState {
  override def toString: String = this match {
    case ShowSeatState.Free => "Free"
    case ShowSeatState.Unavailable => "Unavailable"
    case ShowSeatState.ReservedByOtherClient => "ReservedByOtherClient"
    case ShowSeatState.Reserved => "Reserved"
  }
}

object ShowSeatState {
  final case object Free extends ShowSeatState
  final case object Unavailable extends ShowSeatState
  final case object ReservedByOtherClient extends ShowSeatState
  final case object Reserved extends ShowSeatState
}

final case class ShowSeat(id: SeatId, showId: ShowId, state: ShowSeatState)

sealed trait ClientMessageBody
object ClientMessageBody {
  final case object HiFromServer extends ClientMessageBody
  final case class InitializeState(sessionId: SessionId) extends ClientMessageBody
  final case class UpdateSeat(showSeat: ShowSeat) extends ClientMessageBody
  final case class UpdateSeatBulk(showSeats: List[ShowSeat]) extends ClientMessageBody

  def deserialize(body: ClientMessageBody): String = body match {
    case HiFromServer => """{"SeatsMsg":"HiFromServer"}"""
    case InitializeState(sessionId) => s"""{"SeatsMsg":{"InitializeState":{"SessionId":"${sessionId.value}"}}}"""
    case UpdateSeat(showSeat: ShowSeat) => s"""{"SeatsMsg":{"UpdateSeat":{"Id":{"SeatId":"${showSeat.id.value}"},"ShowId":{"ShowId":"${showSeat.showId.value}"},"State":"${showSeat.state.toString}"}}}"""
    case UpdateSeatBulk(showSeats: List[ShowSeat]) =>
      def toShowSeat(showSeat: ShowSeat): String =
        s"""{"Id":{"SeatId":"${showSeat.id.value}"},"ShowId":{"ShowId":"${showSeat.showId.value}"},"State": "${showSeat.state.toString}"}"""

      val updateSeatBulk = showSeats.map(toShowSeat).mkString(",")
      s"""{"SeatsMsg":{"UpdateSeatBulk":[$updateSeatBulk]}}"""
  }
}

sealed trait ClientMessage {
  def forClient(targetClientId: UUID): Boolean
  def toString: String
}

object ClientMessage {
  case class SendToClient(clientId: UUID, body: ClientMessageBody) extends ClientMessage {
    override def forClient(targetClientId: UUID): Boolean = targetClientId == clientId
    override def toString: String = ClientMessageBody.deserialize(body)
  }

  case class SendToClients(clientIds: Set[UUID], body: ClientMessageBody) extends ClientMessage {
    override def forClient(targetClientId: UUID): Boolean = clientIds.contains(targetClientId)
    override def toString: String = ClientMessageBody.deserialize(body)
  }

  case class SendToAllClients(body: ClientMessageBody) extends ClientMessage {
    override def forClient(targetClientId: UUID): Boolean = true
    override def toString: String = ClientMessageBody.deserialize(body)
  }
}