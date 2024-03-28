import cats.effect.{ExitCode, IO, IOApp, Resource}

import java.io.{BufferedReader, InputStreamReader}
import java.net.{HttpURLConnection, URL}

object ResourceSafetyExercise extends IOApp {
  def createConnection(targetURL: String): IO[HttpURLConnection] = {
    IO.blocking {
      val connection =
        new URL(targetURL).openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      connection
    }
  }

  def readOutput(reader: BufferedReader): IO[String] =
    IO.blocking {
      Iterator
        .continually(reader.readLine)
        .takeWhile(_ != null)
        .mkString("\n")
    }

  def httpGet(targetURL: String): Resource[IO, BufferedReader] = {
    for {
      con <- Resource.make(createConnection(targetURL))(it =>
        IO.blocking(it.disconnect())
      )
      is <- Resource.fromAutoCloseable(IO(con.getInputStream))
      reader <- Resource.fromAutoCloseable(
        IO(new BufferedReader(new InputStreamReader(is)))
      )
    } yield reader
  }

  override def run(args: List[String]): IO[ExitCode] =
    httpGet("https://www.google.com")
      .use(response => readOutput(response))
      .flatMap(body => IO.println(body))
      .as(ExitCode.Success)
}
