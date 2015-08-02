package part3

/**
 * @author zhoufeng
 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }
  
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(a) => a
  }
  
  def depth[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    case Leaf(a) => 0
  }
  
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(a) => Leaf(f(a))
  }
  
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(a) => f(a)
  }
  
  def size2[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)
  
  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)
  
  def depth2[A](t: Tree[A]): Int = fold(t)(a => 0)(1 + _ max _)
  
  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  
}