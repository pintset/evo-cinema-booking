package com.example.cinemabooking

import cats.effect.concurrent.Ref
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import com.example.cinemabooking.domain.EventStore
import com.example.cinemabooking.server._
import com.example.cinemabooking.domain.show.{CommandHandler, Common, Event, Show, ShowState}
import com.example.cinemabooking.server.ClientMessage.SendToClients
import fs2.Stream
import fs2.concurrent.{Queue, Topic}

object BookingApp extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val httpPort = 5000

    for {
      stateRef <- Ref.of[IO, Map[Common.ShowId, Show]](Map.empty)
      eventStoreRef <- Ref.of[IO, Map[Common.ShowId, List[Event]]](Map.empty)

      state = ShowState.apply[IO](stateRef)
      eventStore = EventStore.of[IO, Common.ShowId, Event](eventStoreRef)

      queue <- Queue.unbounded[IO, ServerMessage];
      topic <- Topic[IO, ClientMessage](SendToClients(Set.empty, ClientMessageBody.HiFromServer))

      clientStateRef <- Ref.of[IO, ClientState](ClientState())

      commandHandler = CommandHandler.of[IO](eventStore, state)
      messageServer = MessageServer.of[IO](commandHandler)

      exitCode <- Blocker[IO].use { blocker =>
        val httpStream: Stream[IO, ExitCode] = BookingStream.stream[IO](httpPort, state, queue, topic, blocker)

        val processingStream =
        queue.dequeue
          .evalMap { msg =>
            for {
              currentClientState <- clientStateRef.get
              stateMessage <- messageServer.process(currentClientState, msg)
              clientState <- clientStateRef.modify(_ => stateMessage)
            } yield clientState
          }
          .flatMap(Stream.emits)
          .through(topic.publish)

        Stream(httpStream, processingStream).parJoinUnbounded.compile.drain
          .as(ExitCode.Success)
      }
    } yield exitCode
  }
}
