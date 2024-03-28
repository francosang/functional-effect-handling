import fs2._

object StreamsPure {

  Stream
    .iterate('a')(it => (it + 1).toChar)
    .take(26)
    .toList

  Stream
    .unfold('a')(s => if (s > 'z') None else Some((s, (s + 1).toChar)))
    .toList

  def customIterate[A](initial: A)(next: A => A): Stream[Pure, A] =
    Stream.unfold[Pure, A, A](initial)(s => Some((s, next(s))))

}
