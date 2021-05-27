package com.example.cinemabooking.server

import com.example.cinemabooking.domain.show.Common.{SessionId, ShowId}

import java.util.UUID

sealed trait ClientSession
object ClientSession {
  final case object Inactive extends ClientSession
  final case class Active(showId: ShowId, sessionId: SessionId) extends ClientSession
}

final case class ClientState(clientSessions: Map[UUID, ClientSession])

object ClientState {
  def apply(): ClientState = ClientState(Map.empty)
}
