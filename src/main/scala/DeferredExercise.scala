import cats.effect._
import cats.effect.implicits._
import cats.implicits._

object DeferredExercise extends IOApp {

  trait Runner {
    def run(): IO[Unit]
  }

  class Producer[A](
      name: String,
      deferred: Deferred[IO, A],
      exec: IO[A]
  ) extends Runner {
    override def run(): IO[Unit] = {
      IO.println(s"Producer $name running") *>
        exec.flatMap { a =>
          deferred.complete(a)
        }.void
    }
  }

  class Consumer[A](
      name: String,
      deferred: Deferred[IO, A],
      consume: A => IO[Unit]
  ) extends Runner {
    override def run(): IO[Unit] =
      IO.println(s"Consumer $name running") *>
        deferred.get.flatMap { a => consume(a) }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    Deferred[IO, Int]
      .flatMap { deferred =>
        List(
          new Producer[Int]("Producer", deferred, IO.pure(1)),
          new Consumer[Int]("Consumer", deferred, a => IO.println(a))
        ).parTraverse_(value => value.run())
      }
      .void
      .as(ExitCode.Success)
  }
}
