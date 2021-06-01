package com.example.cinemabooking

import cats.effect.{Blocker, ConcurrentEffect, ContextShift, ExitCode, Sync, Timer}
import cats.implicits._
import com.example.cinemabooking.domain.State
import com.example.cinemabooking.domain.show.Common.ShowId
import com.example.cinemabooking.domain.show.{Event, Show}
import com.example.cinemabooking.server._
import fs2.Stream
import fs2.concurrent.{Queue, Topic}
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.{CORS, Logger}
import org.http4s.server.staticcontent.{FileService, fileService}

import scala.concurrent.ExecutionContext.global

object BookingStream {

  def stream[F[_]: Sync: ConcurrentEffect](port: Int,
                                     state: State[F, ShowId, Show, Event],
                                     queue: Queue[F, ServerMessage], topic: Topic[F, ClientMessage], blocker: Blocker)
                                    (implicit T: Timer[F], cs: ContextShift[F]): Stream[F, ExitCode] =
    for {
      client <- BlazeClientBuilder[F](global).stream
      httpApp = (
        fileService[F](FileService.Config("./dist", blocker)) <+>
          BookingRoutes.httpApiRoutes[F](state, blocker) <+>
          BookingRoutes.socketRoutes[F](queue, topic)
        ).orNotFound

      // With Middlewares in place
      // corsHttpApp = CORS.httpApp(httpApp)
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(port, "0.0.0.0")
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode
}
