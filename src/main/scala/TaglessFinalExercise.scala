import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._

import scala.concurrent.duration.DurationInt

object TaglessFinalExercise extends IOApp {
  case class Image(bytes: List[Byte])

  trait ImagesService[F[_]] {
    def fetchHttp(n: Int): F[List[Image]]
    def fetchDb(n: Int): F[List[Image]]
    def fetchFastest(n: Int): F[List[Image]]
  }

  private object ImagesService {
    def impl[F[_]: Temporal: Sync]: ImagesService[F] =
      new ImagesService[F] {
        override def fetchHttp(n: Int): F[List[Image]] = {
          Sync[F].flatMap(Temporal[F].sleep(1.second)) { _ =>
            List.range(0, n).parTraverse { i =>
              Sync[F].blocking(Image(List(i.toByte)))
            }
          }
        }

        override def fetchDb(n: Int): F[List[Image]] = {
          Sync[F].flatMap(Temporal[F].sleep(2.seconds)) { _ =>
            List.range(0, n).parTraverse { i =>
              Sync[F].blocking(Image(List(i.toByte)))
            }
          }
        }

        override def fetchFastest(n: Int): F[List[Image]] = {
          Sync[F].map(
            Spawn[F]
              .race(fetchHttp(n), fetchDb(n))
          ) { it =>
            it.fold(identity, identity)
          }
        }
      }
  }

  override def run(args: List[String]): IO[ExitCode] =
    ImagesService
      .impl[IO]
      .fetchFastest(10)
      .flatMap(IO.println)
      .as(ExitCode.Success)
}
