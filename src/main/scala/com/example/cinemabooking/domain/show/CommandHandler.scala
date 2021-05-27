package com.example.cinemabooking.domain.show

import cats.Monad
import cats.effect.Sync
import cats.implicits._

import com.example.cinemabooking.domain.EventStore
import Common._
import Show._

trait CommandHandler[F[_]] {
  def execute(showId: ShowId, command: Command): F[Show]
}

object CommandHandler {
  def of[F[_]: Sync: Monad](eventStore: EventStore[F, ShowId, Event],
                            state: ShowState[F]): CommandHandler[F] = {

    (showId: ShowId, command: Command) => for {
      // Get new eventsBuild current state, and pass it to command handler (execute function of aggregate)
      eventsEither <- state.current(showId).map(execute(_, command))

      // Append events to event store (EitherT)
      _ <- eventsEither.map { events => eventStore.append(showId, events) }.sequence

      // Read side (EitherT)
      _ <- eventsEither.map { events => state.evolve(showId, events) }.sequence
      _ <- eventsEither.map { events => Sync[F].delay(println(events)) }.sequence

      // Updated state (with new events)
      updatedState <- state.current(showId)

    } yield updatedState
  }

  // state -> command -> event list
  def execute(show: Show, command: Command): Either[Error, List[Event]] =
    command match {
      case Command.StartSession (sessionId) => Session.start (show, sessionId).left.map(Error.Session)
      case Command.FinishSession(sessionId) => Session.finish(show, sessionId).left.map(Error.Session)
      case Command.AbortSession (sessionId) => Session.abort (show, sessionId).left.map(Error.Session)

      case Command.ReserveSeat(sessionId, seatId) => Seat.reserve(show, sessionId, seatId).left.map(Error.Reservation)
      case Command.ReleaseSeat(sessionId, seatId) => Seat.release(show, sessionId, seatId).left.map(Error.Release)
    }

  // Workflows

  object Session {
    def start(show: Show, sessionId: SessionId): Either[SessionError, List[Event]] =
      Right(List(Event.SessionStarted(sessionId, show.id)))

    def finish(show: Show, sessionId: SessionId): Either[SessionError, List[Event]] =
      Right(List(Event.SessionFinished(sessionId)))

    def abort(show: Show, sessionId: SessionId): Either[SessionError, List[Event]] =
      Right(List(Event.SessionAborted(sessionId)))
  }

  object Seat {
    def reserve(show: Show, sessionId: SessionId, seatId: SeatId): Either[ReservationError, List[Event]] =
      for {
        session <- show.sessions.get(sessionId).toRight(ReservationError.SessionNotFound(sessionId))

        _ <-
          if (session.status == SessionStatus.Started) Right()
          else Left(ReservationError.InactiveSession(session))

        seat <- show.seats.get(seatId).toRight(ReservationError.SeatNotFound(Show.SeatInfo(seatId, show.id)))

        events <- seat match {
          case Show.Seat.Free(info) => Right(List(Event.SeatReserved(Show.ReservedSeat(info, sessionId))))
          case _ => Left(ReservationError.SeatCannotBeReserved(seat))
        }
      } yield events

    def release(show: Show, sessionId: SessionId, seatId: SeatId): Either[ReleaseError, List[Event]] =
      for {
        session <- show.sessions.get(sessionId).toRight(ReleaseError.SessionNotFound(sessionId))

        _ <-
          if (session.status == Show.SessionStatus.Started) Right()
          else Left(ReleaseError.InactiveSession(session))

        seat <- show.seats.get(seatId).toRight(ReleaseError.SeatNotFound(Show.SeatInfo(seatId, show.id)))

        events <- seat match {
          case Show.Seat.Reserved(seat) => Right(List(Event.SeatReleased(seat.info)))
          case _ => Left(ReleaseError.SeatCannotBeReleased(seat))
        }
      } yield events
  }
}
