package part12
import part11.Functor
/**
 * Created by zhoufeng on 15-9-15.
 */
trait Applicative[F[_]] extends Functor[F] {

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def unit[A](a: =>A): F[A]

  //Implement apply with map2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, z) => map2(f(a), z)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))((a, z) => map2(a, z)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    List.fill(n)(fa).foldRight(unit(List[A]()))((a, z) => map2(a, z)(_ :: _))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)


}

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled

    override def sequence[A](a: List[Stream[A]]): Stream[List[A]] =
      a.foldRight(unit(List[A]()))((a, z) => map2(a, z)(_ :: _))

  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = {
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      def unit[A](a: => A) = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
        case (e: Failure[E], _) => e
        case (_, e: Failure[E]) => e
      }
    }
  }

  def product[F[_], G[_]](f: Applicative[F], g: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (f.unit(a), g.unit(a))
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(h: (A, B) => C): (F[C], G[C]) =
        (f.map2(fa._1, fb._1)(h), g.map2(fa._2, fb._2)(h))
    }
  }

  def compose[F[_], G[_]](f: Applicative[F], g: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = f.unit(g.unit(a))
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(h: (A, B) => C): F[G[C]] =
        f.map2(fa, fb)(g.map2(_, _)(h))
    }
  }
}


sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]=Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]



