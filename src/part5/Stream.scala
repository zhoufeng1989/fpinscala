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
  
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(x, xs) if p(x()) => Stream.cons(x(), xs() takeWhile p)
    case _ => Stream.empty
  }
  
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(x, xs) => p(x()) || (xs() exists p)
    case _ => false
  }
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(x, xs) => f(x(), xs().foldRight(z)(f))
    case _ => z
  }
  
  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)
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