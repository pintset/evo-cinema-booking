package com.example.cinemabooking.domain.show

import cats.data.OptionT
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import Common.ShowId
import com.example.cinemabooking.domain.State
import com.example.cinemabooking.Data

trait ShowState[F[_]] extends State[F, ShowId, Show, Event]
object ShowState {
  def apply[F[_]: Sync](stateRef: Ref[F, Map[ShowId, Show]]): ShowState[F] = {
    def init(showId: ShowId): F[Show] =
      (for {
        showDto <- OptionT(Data.Show.getById(showId.value))
        auditoriumSeatsDto <- OptionT(Data.Auditorium.Seat.getById(showDto.auditoriumId))
      } yield auditoriumSeatsDto).value.map(_.get).map { auditoriumSeats =>
        Projection.init(showId, auditoriumSeats)
      }

    val state = State.of[F, ShowId, Show, Event](stateRef, init, Projection.update)

    new ShowState[F] {
      override def current(eventSource: ShowId): F[Show] =
        state.current(eventSource)

      override def evolve(eventSource: ShowId, events: List[Event]): F[Unit] =
        state.evolve(eventSource, events)
    }
  }
}
