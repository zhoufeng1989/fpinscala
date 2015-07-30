package part3

// A trait is an abstract interface that may optionally contain implementations of some methods.
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
	
	def apply[A](as: A*): List[A] = {
    println(as)
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
  }
  
	def tail[A](l: List[A]) = l match {
		case Cons(_, t) => t
		case Nil => throw new Exception("l must not be empty")
	}
  
  def setHead[A](l: List[A], head: A): List[A] = l match {
    case Cons(_, t) => Cons(head, t)
    case Nil => throw new Exception("l must not be empty")
  }
  
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, n) if n <= 0 => l
    case (Nil, _) => Nil
    case (Cons(_, tail), n) => drop(tail, n - 1)
  }
  
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }
  
  /* 
   * for better type inference,when a function definition contains multiple argument groups,
   * type information flows from left to right across these argument groups.
   */
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile2(t)(f)
    case _ => l
  }
  /*
  Note that this definition only copies values until the first list is exhausted, so its runtime 
  * and memory usage are determined only by the length of a1 . The remaining list then just points to a2 . 
  * If we were to implement this same function for two arrays, we’d be forced to copy all the elements 
  * in both arrays into the result. In this case, the immutable linked list is much more efficient than an array
  * 
  */
  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Cons(h, t) => Cons(h, append(t, l2))
    case Nil => l2
  }
  
  /*
   * Because of the structure of a singly linked list, any time we want to replace the tail of
a Cons , even if it’s the last Cons in the list, we must copy all the previous Cons objects.
Writing purely functional data structures that support different operations efficiently
is all about finding clever ways to exploit data sharing.
   */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => throw new Exception("l must not be emtpy")
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Cons(h, t) => f(h, foldRight(t, z)(f))
    case Nil => z
  }
  
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((x, z) => z + 1)
  
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((z, x) => z + 1)
  
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((z, x) => Cons(x, z))
  
  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = 
    foldLeft(reverse(l), z)((z, x) => f(x, z))

  def append2[A](l1: List[A], l2: List[A]):List[A] = 
    foldRight(l1, l2)(Cons(_, _))
    
  def append3[A](l1: List[A], l2: List[A]):List[A] = 
    foldLeft(l1, (b: List[A]) => b)((g, a) => b => g(Cons(a, b)))(l2)
   
  def incrOne(l: List[Int]): List[Int] = foldLeft(l, List[Int]())((z, a) => Cons(a + 1, z))
  
  def doubleToString(l: List[Double]): List[String] = foldLeft(l, List[String]())((z, a) => Cons(a.toString, z))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldLeft(l, List[B]())((z, a) => Cons(f(a), z))
  
  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    foldLeft(l, List[A]())((z, a) => if(f(a)) Cons(a, z) else z)
}