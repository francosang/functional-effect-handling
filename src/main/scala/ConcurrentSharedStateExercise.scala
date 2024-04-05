import cats.effect._
import cats.syntax.all._

object ConcurrentSharedStateExercise extends IOApp {
  case class User(username: String, age: Int, friends: List[User])

  // use ref to hold the current oldest user
  def findOldest(user: User): IO[User] = {
    def go(user: User, ref: Ref[IO, User]): IO[Unit] = {
      ref.update { oldest =>
        if (user.age > oldest.age) user
        else oldest
      } *> user.friends.parTraverse_(go(_, ref))
    }
    
    Ref.of[IO, User](user).flatMap { ref =>
      go(user, ref) *> ref.get
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val a = User("a", 60, Nil)
    val z = User("z", 100, Nil)
    val b = User("b", 35, Nil)
    val c = User("c", 45, Nil)
    val d = User("d", 50, List(a, b, z))
    val e = User("e", 55, List(c))
    val f = User("f", 20, List(d, e))

    findOldest(f).flatTap(IO.println).as(ExitCode.Success)
  }
}
