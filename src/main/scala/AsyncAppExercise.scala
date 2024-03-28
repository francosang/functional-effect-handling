import cats.effect._
import cats.implicits._

object AsyncAppExercise extends IOApp {
  case class User(id: Long, username: String)
  type Error = String

  def findUser(id: Long)(cb: Either[Error, User] => Unit): Unit = {
    if (math.random() < 0.5) cb(Right(User(id, s"User $id")))
    else cb(Left("Something went wrong"))
  }

  def findUserIO(id: Long): IO[User] = {
    // Converts a callback function to an IO
    IO.async_ { cb =>
      findUser(id) {
        case Left(value) => cb(Left(new Exception(value)))
        case Right(user) => cb(Right(user))
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    findUserIO(5)
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }
}
