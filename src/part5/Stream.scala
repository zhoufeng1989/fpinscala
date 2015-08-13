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
  
  def map2[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(x, xs) => Some((f(x()), xs()))
    case _ => None
  }
  
  def take2(n: Int): Stream[A] = Stream.unfold((n, this)) {
    case (n, Cons(x, xs)) if n > 1 => Some(x(), (n - 1, xs()))
    case (1, Cons(x, xs))  => Some(x(), (0, Stream.empty))
    case _ => None
  }
  
  def takeWhile3(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(x, xs) if p(x()) => Some((x(), xs()))
    case _ => None
  }
  
  def zipWith[B, C](s1: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, s1)) {
    case (Cons(x1, xs1), Cons(x2, xs2)) => Some((f(x1(), x2()), (xs1(), xs2())))
    case _ => None
  }
  
  def zip[B](s1: Stream[B]): Stream[(A, B)] = zipWith(s1)((_, _))
  def zipWithAll[B, C](s1: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = Stream.unfold((this, s1)) {
    case (Cons(x1, xs1), Cons(x2, xs2)) => Some((f(Some(x1()), Some(x2())), (xs1(), xs2())))
    case (Cons(x1, xs1), Empty) => Some((f(Some(x1()), None), (xs1(), Stream.empty)))
    case (Empty, Cons(x2, xs2)) => Some((f(None, Some(x2())), (Stream.empty, xs2())))
    case (Empty, Empty) => Some((f(None, None), (Stream.empty, Stream.empty)))
  }
  
  def zipAll[B](s1: Stream[B]): Stream[(Option[A], Option[B])] = zipWithAll(s1)((_, _))
  
  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhile(!_._2.isEmpty) forAll {
    case (x, y) => x == y
  }
  
  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Cons(x, xs) => Some((this, xs()))
    case _ => None
  } append Stream(Stream.empty)
  
  def hasSubsequence[A](s: Stream[A]): Boolean = 
    tails exists (_ startsWith s)
  
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = 
    foldRight((z, Stream(z)))((a, p0) => {
     lazy val p1 = p0
     val b2 = f(a, p1._1)
     (b2, Stream.cons(b2, p1._2))
    })._2
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