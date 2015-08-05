package part4

/**
 * @author zhoufeng
 */
sealed trait Option [+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }
  
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }
  
  def flatMap2[B](f: A => Option[B]): Option[B] = this map f getOrElse None
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case None => ob
  }
  
  def orElse2[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob
  
  def fiter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
  
  def filter2(f: A => Boolean): Option[A] = this flatMap (a => if(f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
  
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean((xs map (x => math.pow(x - m, 2)))))
  }
  
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
  
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(x), Some(y)) => Some(f(x, y))
  }
  
  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a flatMap {x => (b map (y => f(x, y)))}
  
  def sequence[A](l: List[Option[A]]): Option[List[A]] =  l match {
    case Nil => Some(Nil)
    case x :: xs => x flatMap (xx => sequence(xs) map (xx :: _))
  }
  
  def traverse[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] = sequence(l map f)
  
  def traverse2[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] = l match {
    case Nil => Some(Nil)
    case x :: xs => f(x) flatMap (xx => traverse2(xs)(f) map (xx :: _))
  }
}