package com.example.cinemabooking

import cats.effect.{Blocker, Concurrent, ContextShift, Sync}
import cats.implicits._
import com.example.cinemabooking.domain.show.Common.ShowId
import com.example.cinemabooking.domain.show.{Event, Show}
import com.example.cinemabooking.domain.State
import com.example.cinemabooking.server._
import fs2.Stream
import fs2.concurrent.Topic
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.{Close, Text}
import org.http4s._

import scala.language.implicitConversions

object BookingRoutes {
  import fs2.Pipe
  import fs2.concurrent.Queue
  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  def socketRoutes[F[_]: Sync: Concurrent](queue: Queue[F, ServerMessage],
                                           topic: Topic[F, ClientMessage]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "socket" / UUIDVar(clientId) =>

        // Routes messages from our "topic" to a WebSocket
        val toClient: Stream[F, WebSocketFrame.Text] =
          topic
            .subscribe(1000)
            .filter(_.forClient(clientId))
            .map { msg => Text(msg.toString) }

        // Function that converts a stream of one type to another. Effectively an external "map" function
        def processInput(wsfStream: Stream[F, WebSocketFrame]): Stream[F, Unit] = {
          // Stream of initialization events for a user
          val entryStream: Stream[F, ServerMessage] = Stream.emits(Seq(ServerMessage(clientId, ServerMessageBody.SayHi)))

          // Stream that transforms between raw text from the client and parsed InputMessage objects
          val parsedWebSocketInput: Stream[F, ServerMessage] =
            wsfStream
              .collect {
                case Text(text, _) => ServerMessage.parse(clientId, text)

                // Convert the terminal WebSocket event to a User disconnect message
                case Close(_) => ServerMessage(clientId, ServerMessageBody.Disconnect(clientId))
              }

          // Create a stream that has all of the user input sandwiched between the entry and disconnect messages
          (entryStream ++ parsedWebSocketInput).through(queue.enqueue)
          // parsedWebSocketInput.through(queue.enqueue)
        }

        // WebSocketBuilder needs a "pipe" which is a type alias for a stream transformation function like processInput above
        // This variable is not necessary to compile, but is included to clarify the exact type of Pipe.
        val inputPipe: Pipe[F, WebSocketFrame, Unit] = processInput

        // Build the WebSocket handler
        WebSocketBuilder[F].build(toClient, inputPipe)
    }
  }

  def httpApiRoutes[F[_]: Sync: ContextShift](state: State[F, ShowId, Show, Event], blocker: Blocker): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    implicit def getState(showId: ShowId): F[Show] = state.current(showId)

    def static(file: String, blocker: Blocker, request: Request[F]): F[Response[F]] =
      StaticFile.fromResource("/" + file, blocker, Some(request)).getOrElseF(NotFound())

    def optResponse[A](opt: Option[A])(implicit encoder: EntityEncoder[F, A]): F[Response[F]] = {
      opt.map(Ok(_)).getOrElse(NotFound())
    }

    HttpRoutes.of[F] {
      case GET -> Root / "api" / "movies" =>
        Ok(Data.Movie.getAll)

      case GET -> Root / "api" / "movies" / IntVar(id) =>
        Data.Movie.getById(id).flatMap(optResponse(_))

      case GET -> Root / "api" / "movies" / IntVar(id) / "schedule" =>
        Data.Schedule.getById(id).flatMap(optResponse(_))

      case request @ GET -> Root / "api" / "movies" / IntVar(id) / "poster" =>
        static(s"data/posters/$id.jpg", blocker, request)

      case GET -> Root / "api" / "theatres" =>
        Ok(Data.Theatre.getAll)

      case GET -> Root / "api" / "theatres" / UUIDVar(id) =>
        Data.Theatre.getById(id).flatMap(optResponse(_))

      case GET -> Root / "api" / "auditoriums" =>
        Ok(Data.Auditorium.getAll)

      case GET -> Root / "api" / "auditoriums" / UUIDVar(id) =>
        Data.Auditorium.getById(id).flatMap(optResponse(_))

      case GET -> Root / "api" / "shows" =>
        Ok(Data.Show.getAll)

      case GET -> Root / "api" / "shows" / UUIDVar(id) =>
        Data.Show.getById(id).flatMap(optResponse(_))

      case GET -> Root / "api" / "shows" / UUIDVar(id) / "info" =>
        Data.Show.Info.getById(id).flatMap(optResponse(_))
    }
  }
}