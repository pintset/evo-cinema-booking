package com.example.cinemabooking.server

import com.example.cinemabooking.domain.show.Common.{SeatId, ShowId}
import io.circe.generic.auto._
import io.circe.parser._

import java.util.UUID

final case class ServerMsg(command: String, data: Option[UUID])

sealed trait ServerMessageBody
object ServerMessageBody {
  final case object SayHi extends ServerMessageBody
  final case class Disconnect(clientId: UUID) extends ServerMessageBody

  final case class StartSession(showId: ShowId) extends ServerMessageBody
  final case object FinishSession extends ServerMessageBody
  final case object AbortSession extends ServerMessageBody
  final case class ReserveSeat(seatId: SeatId) extends ServerMessageBody
  final case class ReleaseSeat(seatId: SeatId) extends ServerMessageBody
}

final case class ServerMessage(clientId: UUID, body: ServerMessageBody)
object ServerMessage {
  def parse(clientId: UUID, text: String): ServerMessage =
    decode[Array[String]](text).flatMap { msgArray =>
      decode[ServerMsg](msgArray(1)).map {
        case ServerMsg("SayHi", None) => ServerMessageBody.SayHi
        case ServerMsg("StartSession", Some(showId)) => ServerMessageBody.StartSession(ShowId(showId))
        case ServerMsg("FinishSession", None) => ServerMessageBody.FinishSession
        case ServerMsg("AbortSession", None) => ServerMessageBody.AbortSession
        case ServerMsg("ReserveSeat", Some(seatId)) => ServerMessageBody.ReserveSeat(SeatId(seatId))
        case ServerMsg("ReleaseSeat", Some(seatId)) => ServerMessageBody.ReleaseSeat(SeatId(seatId))
      }.map { body => ServerMessage(clientId, body) }
    }.toTry.get
}
