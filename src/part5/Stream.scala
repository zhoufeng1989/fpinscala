package part5

/**
 * @author zhoufeng
 */
sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(x, xs) => x() :: xs().toList
  }
  
  def take(n: Int): Stream[A] = this match {
    case Cons(x, xs) if n > 1 => Stream.cons(x(), xs() take (n - 1))
    case Cons(x, xs) if n == 1 => Stream.cons(x(), Stream.empty)
    case _ => Stream.empty
  }
  
  def drop(n: Int): Stream[A] = this match {
    case Cons(x, xs) if n > 0 => xs() drop (n - 1)
    case _ => this
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }
  
  def empty[A]: Stream[A] = Empty
  
  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}