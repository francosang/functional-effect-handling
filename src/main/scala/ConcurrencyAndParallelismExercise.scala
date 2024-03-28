import cats._
import cats.effect._
import cats.implicits._
import cats.effect.implicits

import scala.concurrent.duration._

object ConcurrencyAndParallelismExercise extends IOApp {

  case class Quote(author: String, text: String)

  def fetchHttp(n: Int): IO[List[Quote]] =
    IO.sleep(100.millis) *>
      (1 to n).toList.map(i => Quote(s"Author $i", s"Text $i")).pure[IO]

  def fetchDn(n: Int): IO[List[Quote]] =
    IO.sleep(10.millis) *>
      (1 to n).toList.map(i => Quote(s"Author $i", s"Text $i")).pure[IO]

  def fetchAuthorAge(author: String): IO[Int] =
    IO.sleep(100.millis) *> IO((math.random() * 100).toInt)

  override def run(args: List[String]): IO[ExitCode] = {
    val n = 3

    IO.race(fetchHttp(n), fetchHttp(n))
      .map(it => it.fold(identity, identity))
      .flatMap(quotes => quotes.map(_.author).parTraverse(fetchAuthorAge))
      .map(ages => ages.sum / ages.length)
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }
}
