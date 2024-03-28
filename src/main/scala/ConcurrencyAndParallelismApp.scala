import cats._
import cats.effect._
import cats.implicits._
import cats.effect.implicits

object ConcurrencyAndParallelismApp extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {

    // IO.race


    List(1, 2, 3)
      .traverse { i =>
        IO.println(s"Task $i")
      }
      .as(ExitCode.Success)
  }
}
