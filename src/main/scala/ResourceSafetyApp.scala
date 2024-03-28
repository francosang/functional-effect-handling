import cats.effect._

import java.io._

object ResourceSafetyApp extends IOApp {

  case class Person(name: String, age: Int)

  trait RowEncoder[A] {
    def encode(a: A): String
  }
  implicit val personEncoder: RowEncoder[Person] = (p: Person) =>
    s"${p.name},${p.age}"

  def writeAll[A](objects: List[A], file: File)(implicit
      encoder: RowEncoder[A]
  ): IO[Unit] = {
    /*
    UNSAFE
    for {
      fw <- IO.blocking(new FileWriter(file))
      contents = objects.map(encoder.encode).mkString("\n")
      _ <- IO.blocking(fw.write(contents))
      _ <- IO.blocking(fw.flush())
      _ <- IO.blocking(fw.close())
    } yield ()
     */
    // SAFE implementation
    def use(fw: FileWriter): IO[Unit] = {
      val contents = objects.map(encoder.encode).mkString("\n")
      IO.blocking(fw.write(contents)) *> IO.blocking(fw.flush())
    }

    def release(fw: FileWriter): IO[Unit] =
      IO.blocking(fw.close())

    IO.blocking(new FileWriter(file)).bracket(use)(release)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val file = new File("test")
    val objects = List(Person("Leandro", 31), Person("Maria", 24))

    writeAll(objects, file).as(ExitCode.Success)
  }
}
