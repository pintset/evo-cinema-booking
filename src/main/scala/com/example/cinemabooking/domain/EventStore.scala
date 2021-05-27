package com.example.cinemabooking.domain

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

trait EventStore[F[_], S, E] {
  def getStream(eventSource: S): F[List[E]]
  def append(eventSource: S, events: List[E]): F[Unit]
}

object EventStore {
  def of[F[_]: Sync: Monad, S, E](eventStoreRef: Ref[F, Map[S, List[E]]]): EventStore[F, S, E] = {
    def getEvents(eventSource: S): F[List[E]] =
      eventStoreRef.get.map { eventStore => eventStore.getOrElse(eventSource, List.empty) }

    new EventStore[F, S, E] {
      override def getStream(eventSource: S): F[List[E]] =
        getEvents(eventSource).map(_.reverse)

      override def append(eventSource: S, events: List[E]): F[Unit] =
        getEvents(eventSource).flatMap { eventsHistory =>
          eventStoreRef.update { eventStore =>
            eventStore + (eventSource -> eventsHistory.concat(events))
          }
        }
    }
  }
}