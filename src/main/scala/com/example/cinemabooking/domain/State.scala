package com.example.cinemabooking.domain

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

trait State[F[_], S, ST, E] {
  def current(eventSource: S): F[ST]
  def evolve(eventSource: S, events: List[E]): F[Unit]
}

object State {
  def of[F[_]: Sync: Monad, S, ST, E](stateRef: Ref[F, Map[S, ST]], init: S => F[ST], folder: (ST, E) => ST): State[F, S, ST, E] = {
    new State[F, S, ST, E] {
      override def current(eventSource: S): F[ST] =
        stateRef.get.flatMap { states =>
          states.get(eventSource) match {
            case Some(state) => Sync[F].pure(state)
            case _ => init(eventSource)
          }
        }

      override def evolve(eventSource: S, events: List[E]): F[Unit] =
        current(eventSource).flatMap { currentState =>
          stateRef.update { states =>
            val newState = states + (eventSource -> events.foldLeft(currentState)(folder))
            newState
          }
        }
    }
  }
}