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