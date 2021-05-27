package com.example.cinemabooking

import cats.Monad
import cats.effect.Sync
import com.example.cinemabooking.domain.show
import io.circe.Decoder

import java.time.{LocalDate, ZonedDateTime}
import scala.io.Source
import scala.util.Try

private object Parse {
  def dataFile(fileName: String) = s"dist/data/$fileName"

  def fileF[A, B, C](fileName: String, f: List[A] => Map[B, C])(implicit decoder: Decoder[List[A]]): Map[B, C] =
    Try {
      val src = Source.fromResource(dataFile(fileName))
      val s = src.mkString
      src.close()
      s
    }
      .toEither.flatMap(io.circe.parser.decode[List[A]])
      .map(items => f(items))
      .toTry.get

  def file[A, B, C](fileName: String, tupled: A => (B, C))(implicit decoder: Decoder[List[A]]): Map[B, C] = {
    def f(data: List[A]): Map[B, C] = data.map(tupled).toMap
    fileF(fileName, f)
  }
}

private object Get {
  def all[F[_]: Sync, A, B](data: Map[A, B]): F[List[B]] = Sync[F].delay(data.values.toList)
  def byId[F[_]: Sync, A, B](data: Map[A, B], id: A): F[Option[B]] = Sync[F].delay(data.get(id))
}

object Data {
  import io.circe.generic.auto._
  import java.util.UUID

  final case class Movie(id: Int, title: String, description: String)

  object Movie {
    val map: Map[Int, Movie] = Parse.file("movies.json", (m: Movie) => (m.id, m))

    def getAll[F[_]: Sync]: F[List[Movie]] = Get.all(map)
    def getById[F[_]: Sync](id: Int): F[Option[Movie]] = Get.byId(map, id)
  }

  final case class Theatre(id: UUID, name: String)

  object Theatre {
    val map: Map[UUID, Theatre] = Parse.file("theatres.json", (t: Theatre) => (t.id, t))

    def getAll[F[_]: Sync]: F[List[Theatre]] = Get.all(map)
    def getById[F[_]: Sync](id: UUID): F[Option[Theatre]] = Get.byId(map, id)
  }

  final case class Auditorium(id: UUID, theatreId: UUID, name: String)

  object Auditorium {
    val map: Map[UUID, Auditorium] = Parse.file("auditoriums.json", (a: Auditorium) => (a.id, a))

    def getAll[F[_]: Sync]: F[List[Auditorium]] = Get.all(map)
    def getById[F[_]: Sync](id: UUID): F[Option[Auditorium]] = Get.byId(map, id)

    final case class Seat(seatId: UUID, row: Int, number: Int)

    object Seat {
      def apply(seatId: UUID, auditoriumId: UUID, row: Int, number: Int): (UUID, Seat) =
        (auditoriumId, Seat(seatId, row, number))

      implicit val decoder: Decoder[(UUID, Seat)] =
        Decoder.forProduct4("seatId", "auditoriumId", "row", "number")(apply)

      val map: Map[UUID, List[Seat]] =
        Parse.fileF("audSeats.json", (ss: List[(UUID, Seat)]) => ss.groupBy { case (id, _) => id })
          .map { case (id, seats) => (id, seats.map { case (_, seat) => seat }) }

      def getById[F[_]: Sync](auditoriumId: UUID): F[Option[List[Seat]]] = Get.byId(map, auditoriumId)
    }
  }

  final case class Show(id: UUID, movieId: Int, auditoriumId: UUID, dateTime: ZonedDateTime)

  object Show {
    val map: Map[UUID, Show] = Parse.file("shows.json", (s: Show) => (s.id, s))

    def getAll[F[_]: Sync]: F[List[Show]] = Get.all(map)
    def getById[F[_]: Sync](id: UUID): F[Option[Show]] = Get.byId(map, id)

    sealed trait SeatState {
      import SeatState._

      override def toString: String = this match {
        case Free => "free"
        case Unavailable => "unavailable"
        case Reserved => "reserved"
      }
    }

    object SeatState {
      final object Free extends SeatState
      final object Unavailable extends SeatState
      final object Reserved extends SeatState
    }

    final case class Seat(seatId: UUID, row: Int, number: Int, state: String)

    final case class Info(showId: UUID, title: String, posterUrl: String, auditorium: String,
                           theatre: String, dateTime: ZonedDateTime, seats: List[Seat])

    object Info {
      import com.example.cinemabooking.domain.show.Common._
      import com.example.cinemabooking.domain.show.Show._
      import cats.implicits._

      def getById[F[_]: Sync: Monad](showId: UUID)(implicit getState: ShowId => F[show.Show]): F[Option[Info]] = {
        (for {
          show <- map.get(showId)
          movie <- Movie.map.get(show.movieId)
          auditorium <- Auditorium.map.get(show.auditoriumId)
          theatre <- Theatre.map.get(auditorium.theatreId)
          auditoriumSeats <- Auditorium.Seat.map.get(auditorium.id)
        } yield (show, movie, auditorium, theatre, auditoriumSeats)).map { case (show, movie, auditorium, theatre, auditoriumSeats) =>
          getState(ShowId(showId)).map { domainShow =>
            def seatState(seatId: SeatId): SeatState =
              domainShow.seats(seatId) match {
                case domain.show.Show.Seat.Free(_) => SeatState.Free
                case domain.show.Show.Seat.Unavailable(_) => SeatState.Unavailable
                case domain.show.Show.Seat.Reserved(_) | domain.show.Show.Seat.Paid(_) => SeatState.Reserved
              }

            val seats = auditoriumSeats.map { seat =>
              Show.Seat(seat.seatId, seat.row, seat.number, seatState(SeatId(seat.seatId)).toString)
            }

            // TODO: Fix this before pushing
            Info(show.id, movie.title, s"http://localhost:5000/api/movies/${movie.id}/poster",
              auditorium.name, theatre.name, show.dateTime, seats)
          }
        }
      }.sequence
    }
  }

  final case class AuditoriumInfo(showId: UUID, name: String, time: ZonedDateTime)
  final case class TheatreInfo(name: String, auditoriums: List[AuditoriumInfo])
  final case class Schedule(date: LocalDate, theatres: List[TheatreInfo])

  object Schedule {
    val map: Map[Int, List[Schedule]] = {
      val theatreMap = Theatre.map.view.mapValues(_.name)
      val auditoriumMap = Auditorium.map.view.mapValues(a => (a.name, a.theatreId))
      val shows = Show.map.values

      final case class Temp(id: UUID, movieId: Int, date: LocalDate,
                            theatreName: String, auditoriumName: String, time: ZonedDateTime)

      shows
        .filter { _.dateTime.compareTo(ZonedDateTime.now()) > 0 }
        .map { s => Temp(s.id, s.movieId, s.dateTime.toLocalDate,
          theatreMap(auditoriumMap(s.auditoriumId)._2), auditoriumMap(s.auditoriumId)._1, s.dateTime) }
        .groupBy(_.movieId)
        .map { case (id, shows) =>
          val schedules = shows.groupBy(_.date).map { case (date, shows) =>
                val theatres = shows.groupBy(_.theatreName).map { case (theatreName, shows) =>
                  val auditoriums = shows.map { s => AuditoriumInfo(s.id, s.auditoriumName, s.time) }
                  TheatreInfo(theatreName, auditoriums.toList)
                }
                Schedule(date, theatres.toList)
              }
          (id, schedules.toList)
        }
    }

    def getById[F[_]: Sync](movieId: Int): F[Option[List[Schedule]]] = Get.byId(map, movieId)
  }
}
