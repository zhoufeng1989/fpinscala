package part10

/**
 * Created by zhoufeng on 15-8-29.
 */


trait Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}


object Monoid{
  val stringMonoid  = new Monoid[String] {
    def op(x: String, y: String) = x + y
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(x: List[A], y: List[A]) = x ++ y
    val zero = Nil
  }

  val intAddition = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(x: Int, y: Int) = x * y
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x || y
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x && y
    val zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(x: A => A, y: A => A): A => A = a => x(y(a))
    val zero = (a: A) => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def concatenate[A](as: List[A], m: Monoid[A]) =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](s: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val length = s.length
    if (length == 0) {
      m.zero
    }
    else if (length == 1) {
      f(s(0))
    }
    else {
      val (l, r) = s.splitAt(length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def ordered(s: IndexedSeq[Int]): Boolean = {
    if (s.length <= 1)
      true
    else {
      val l = s.toList
      foldMap(l zip l.tail, booleanAnd)(a => a._1 < a._2)
    }
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(x: WC, y: WC) = (x, y) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
      case (Part(l, w, r), Stub(b)) => Part(l, w, r + b)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if((r1 + l2).isEmpty) 0 else 1), r2)
    }

    val zero = Stub("")
  }

  def count(chars: String): Int = {
    def wc(c: Char): WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    def unstub(s: String) = s.length min 1

    foldMapV(chars.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(x: (A, B), y: (A, B)): (A, B) = (a.op(x._1, y._1), b.op(x._2, y._2))
    val zero = (a.zero, b.zero)
  }

  def functionMonoid[A, B](x: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f: A => B, g: A => B) = a => x.op(f(a), g(a))
    val zero: A => B = a => x.zero
  }
}
