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
  
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
  
  def takeWhile2(p: A => Boolean): Stream[A] = 
    foldRight[Stream[A]](Stream.empty)((a, b) => (if(p(a)) Stream.cons(a, b) else Stream.empty))
    
  def headOption: Option[A] = foldRight[Option[A]](None)((a, b) => Some(a))
  
  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
  
  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => (if (f(a)) Stream.cons(a, b) else b))
  
  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)(Stream.cons(_, _))
  
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a) append(b))
  
  def find(p: A => Boolean): Option[A] = this.filter(p).headOption
  
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
  
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
 
  val fibs: Stream[Int] = {
      def go(f0: Int, f1: Int): Stream[Int] = 
        cons(f0, go(f1, f0 + f1))
      go(0, 1)
    }
 
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  val fibs2 = unfold((0, 1)){case (f0, f1) => Some((f0, (f1, f0 + f1)))}
  
  val ones = unfold(1)(a => Some(a, a))
  
  def from2(n: Int) = unfold(n)(a => Some((a, a + 1)))
}