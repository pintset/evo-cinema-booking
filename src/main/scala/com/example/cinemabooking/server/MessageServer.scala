package com.example.cinemabooking.server

import cats.Monad
import cats.effect.Sync
import com.example.cinemabooking.domain.show.Common.{SessionId, ShowId}
import com.example.cinemabooking.domain.show.Show._
import com.example.cinemabooking.domain.show._

import java.util.UUID

trait MessageServer[F[_]] {
  def process(clientState: ClientState, msg: ServerMessage): F[(ClientState, Seq[ClientMessage])]
}

object MessageServer {
  import cats.implicits._

  def of[F[_]: Sync: Monad](handler: CommandHandler[F]): MessageServer[F] = {

    def toShowSeat(seat: Seat, isForLocalClient: Boolean): ShowSeat = seat match {
      case Show.Seat.Free(info) => ShowSeat(info.id, info.showId, ShowSeatState.Free)
      case Show.Seat.Unavailable(info) => ShowSeat(info.id, info.showId, ShowSeatState.Unavailable)
      case Show.Seat.Reserved(seat) if isForLocalClient => ShowSeat(seat.info.id, seat.info.showId, ShowSeatState.Reserved)
      case Show.Seat.Reserved(seat) => ShowSeat(seat.info.id, seat.info.showId, ShowSeatState.ReservedByOtherClient)
      case Show.Seat.Paid(info) => ShowSeat(info.id, info.showId, ShowSeatState.Reserved)
    }

    (clientState: ClientState, msg: ServerMessage) => {
      val clientSession = clientState.clientSessions.getOrElse(msg.clientId, ClientSession.Inactive)

      def clientDispatch(body: ClientMessageBody): Seq[ClientMessage] =
        Seq(ClientMessage.SendToClient(msg.clientId, body))

      def clientsDispatch(showId: ShowId, sessionId: SessionId, body: ClientMessageBody): Seq[ClientMessage] = {
        val clientIds = clientState.clientSessions.toList
          .mapFilter {
            case (cId, ClientSession.Active(shId, seId)) if shId == showId && seId != sessionId && cId != msg.clientId => Some(cId)
            case _ => None
          }.toSet

        Seq(ClientMessage.SendToClients(clientIds, body))
      }

      val result: F[(ClientSession, Seq[ClientMessage])] =
        (msg.body, clientSession) match {
          case (ServerMessageBody.SayHi, clientSession) =>
            Sync[F].pure(clientSession, clientDispatch(ClientMessageBody.HiFromServer))

          case (ServerMessageBody.StartSession(showId), ClientSession.Inactive) =>
            val sessionId = SessionId(UUID.randomUUID())
            handler.execute(showId, Command.StartSession(sessionId)).map { _ =>
              (ClientSession.Active(showId, sessionId), clientDispatch(ClientMessageBody.InitializeState(sessionId)))
            }

          case (ServerMessageBody.FinishSession, ClientSession.Active(showId, sessionId)) =>
            handler.execute(showId, Command.StartSession(sessionId)).map { _ =>
              (ClientSession.Inactive, Seq.empty)
            }

          case (ServerMessageBody.AbortSession, ClientSession.Active(showId, sessionId)) =>
            handler.execute(showId, Command.AbortSession(sessionId)).map { state =>
              val seats = state.seats.values.toList.mapFilter {
                case Show.Seat.Free(info) => Some(ShowSeat(info.id, info.showId, ShowSeatState.Free))
                case _ => None
              }

              (ClientSession.Inactive, clientsDispatch(showId, sessionId, ClientMessageBody.UpdateSeatBulk(seats)))
            }

          case (ServerMessageBody.Disconnect(_), ClientSession.Active(showId, sessionId)) =>
            handler.execute(showId, Command.AbortSession(sessionId)).map { state =>
              val seats = state.seats.values.toList.mapFilter {
                case Show.Seat.Free(info) => Some(ShowSeat(info.id, info.showId, ShowSeatState.Free))
                case _ => None
              }

              (ClientSession.Inactive, clientsDispatch(showId, sessionId, ClientMessageBody.UpdateSeatBulk(seats)))
            }

          case (ServerMessageBody.Disconnect(_), _) =>
            Sync[F].pure((ClientSession.Inactive, Seq.empty))

          case (ServerMessageBody.ReserveSeat(seatId), ClientSession.Active(showId, sessionId)) =>
            handler.execute(showId, Command.ReserveSeat(sessionId, seatId)).map { state =>
              state.seats.get(seatId).map { seat =>
                val clientMessage = clientDispatch(ClientMessageBody.UpdateSeat(toShowSeat(seat, isForLocalClient = true)))
                val otherClientM = clientsDispatch(showId, sessionId, ClientMessageBody.UpdateSeat(toShowSeat(seat, isForLocalClient = false)))

                (clientSession, clientMessage ++ otherClientM)
              }.getOrElse((clientSession, Seq.empty))
            }

          case (ServerMessageBody.ReleaseSeat(seatId), ClientSession.Active(showId, sessionId)) =>
            handler.execute(showId, Command.ReleaseSeat(sessionId, seatId)).map { state =>
              state.seats.get(seatId).map { seat =>
                val clientMessage = clientDispatch(ClientMessageBody.UpdateSeat(toShowSeat(seat, isForLocalClient = true)))
                val otherClientM = clientsDispatch(showId, sessionId, ClientMessageBody.UpdateSeat(toShowSeat(seat, isForLocalClient = false)))

                (clientSession, clientMessage ++ otherClientM)
              }.getOrElse((clientSession, Seq.empty))
            }

          case _ => Sync[F].pure((ClientSession.Inactive, Seq.empty))
        }

      result.map { case (clientSession, messages) =>
        val updatedSessions = clientState.clientSessions + (msg.clientId -> clientSession)
        val newState = clientState.copy(clientSessions = updatedSessions)
        (newState, messages)
      }
    }
  }
}
