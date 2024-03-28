import cats.effect._
import fs2.Stream

object StreamsEffectful extends IOApp.Simple {

  override def run: IO[Unit] = {

    val data = (1 to 100).toList
    val pageSize = 20

    def fetchData(pageNumber: Int): IO[List[Int]] = {
      val start = pageNumber * pageSize
      val end = start + pageSize

      IO.println(s"Fetching page $pageNumber").as(data.slice(start, end))
    }

    def fetchAll(): Stream[IO, Int] = {
      Stream
        .unfoldEval(0) { pageNumber =>
          val pep = fetchData(pageNumber).flatMap(page => {
            if (page.length < pageSize) IO.println(s"Not emitting").as(None)
            else
              IO.println(s"Emitting ${page.size} elements")
                .as(Some((page, pageNumber + 1)))
          })
          pep
        }
        .flatMap(Stream.emits)
    }

    fetchAll().compile.toList.flatMap(IO.println)
  }
}
