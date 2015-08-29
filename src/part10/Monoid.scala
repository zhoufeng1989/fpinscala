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
}

