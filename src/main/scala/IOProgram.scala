import cats.effect.{ExitCode, IO, IOApp}

object IOProgram extends IOApp {
  val computation1 =
    IO.println("Hello, world!") *> IO.println("Goodbye, world!")

  val computation2 =
    IO.println("Hello, world!") >> IO.println("Goodbye, world!")

  def echoForever: IO[Unit] = (for {
    _ <- IO.println("Type something: ")
    str <- IO.readLine
    _ <- IO.println(str)
  } yield ()).foreverM

  def main: IO[Unit] =
    computation1 *>
      computation2 *>
      echoForever

  override def run(args: List[String]): IO[ExitCode] =
    main.as(ExitCode.Success)
}
