package com.example.cinemabooking

import cats.effect.concurrent.Ref
import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, Timer}
import cats.implicits._
import com.example.cinemabooking.domain.show.{Event, Show}
import com.example.cinemabooking.domain.State
import com.example.cinemabooking.server._
import com.example.cinemabooking.domain.show.Common.ShowId
import fs2.Stream
import fs2.concurrent.{Queue, Topic}
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.{CORS, Logger}

import scala.concurrent.ExecutionContext.global

object BookingStream {

  def stream[F[_]: ConcurrentEffect](port: Int,
                                     state: State[F, ShowId, Show, Event],
                                     queue: Queue[F, ServerMessage], topic: Topic[F, ClientMessage])
                                    (implicit T: Timer[F], cs: ContextShift[F]): Stream[F, ExitCode] =
    for {
      client <- BlazeClientBuilder[F](global).stream
      httpApp = (
        BookingRoutes.helloWorldRoutes[F](state) <+>
        BookingRoutes.socketRoutes[F](queue, topic)
      ).orNotFound

      // With Middlewares in place
      corsHttpApp = CORS.httpApp(httpApp)
      finalHttpApp = Logger.httpApp(true, true)(corsHttpApp)

      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(port, "0.0.0.0")
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode
}
