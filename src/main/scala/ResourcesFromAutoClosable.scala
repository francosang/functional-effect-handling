import cats.effect._

import java.io._

object ResourcesFromAutoClosable extends IOApp {

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

  def encryptFile(sourceFile: File, destFile: File): IO[Unit] = {
    // Define resources
    val acquireReader = IO.blocking(new FileInputStream(sourceFile))
    val acquireWriter = IO.blocking(new FileOutputStream(destFile))


    // Resource.fromAutoCloseable() will close the resource automatically
    // with no need to pass close function but we are no longer on control of it
    val readerRes = Resource.fromAutoCloseable(acquireReader)
    val writerRes = Resource.fromAutoCloseable(acquireWriter)

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
