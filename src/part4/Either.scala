package part4

/**
 * @author zhoufeng
 */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]