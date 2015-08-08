package part4

/**
 * @author zhoufeng
 */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))
  }
  
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => f(value)
  }
  
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(value) => b
    case Right(value) => this
  }
  
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    this flatMap (a => (b map (f(a, _))))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case x :: xs => x flatMap (xx => (sequence(xs) map (xx :: _)))
  }
  
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case x :: xs => f(x) flatMap (xx => traverse(xs)(f) map (xx :: _))
  }
  
  def traverse2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    as.foldRight[Either[E, List[B]]](Right(Nil))((x, z) => f(x) flatMap (xx => (z map (xx :: _))))
    
  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)
}
