import java.io._
import cats.effect._

object ResourcesMake extends IOApp {

  def write(value: Array[Byte], stream: FileOutputStream): IO[Unit] =
    IO.println("Writing...") *> IO.blocking(stream.write(value))

  def read(stream: FileInputStream): IO[Array[Byte]] =
    IO.println("Reading...") *> IO.blocking {
      Iterator
        .continually(stream.read)
        .takeWhile(_ != -1)
        .map(_.toByte)
        .toArray
    }

  def encrypt(value: Array[Byte]): IO[Array[Byte]] =
    IO.println("Encrypting...") *> IO.pure(value.map(b => (b + 1).toByte))

  def close[A <: AutoCloseable](msg: String)(ac: A): IO[Unit] =
    IO.println(s"Closing $msg...") *> IO.blocking(ac.close())

  def encryptFile(sourceFile: File, destFile: File): IO[Unit] = {
    // Define resources
    val acquireReader = IO.blocking(new FileInputStream(sourceFile))
    val acquireWriter = IO.blocking(new FileOutputStream(destFile))

    // -- Boring way of using safe resources with bracket --
    /*
    acquireReader.bracket { reader =>
      acquireWriter.bracket { writer =>
        read(reader).flatMap(encrypt).flatMap(write(_, writer))
      }(close)
    }(close)
     */

    // -- Using Cats Resource --

    // Needs an effect that acquires a resource and a function to close/release it
    val readerRes =
      Resource.make[IO, FileInputStream](acquireReader)(close("reader"))
    val writerRes =
      Resource.make[IO, FileOutputStream](acquireWriter)(close("writer"))

    // Combine the resources with flatMap
    // The result is a composition of both resources in a tuple
    val readerAndWriterRes: Resource[IO, (FileInputStream, FileOutputStream)] =
      readerRes.flatMap(r => writerRes.map(w => (r, w)))

    // Release in reverse order
    // First release the writer, then the reader-
    readerAndWriterRes.use { case (reader, writer) =>
      read(reader).flatMap(encrypt).flatMap(write(_, writer)) *>
        IO.println("Done")
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val source = new File("source")
    val dest = new File("dest")
    encryptFile(source, dest).as(ExitCode.Success)
  }
}
