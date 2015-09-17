package part11
import part11.Functor
/**
 * Created by zhoufeng on 15-9-13.
 */
// All monads are functors
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => (map(mb)(b => f(a, b))))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, lma) => map2(ma, lma)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, lmb) => map2(f(a), lmb)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    def g = (_: Unit) => ma
    compose(g, f)(())
  }

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  def flatMap2[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))
}

object Monad {
  val optionMonad = new Monad[Option] {
    def unit[A](a: =>A) = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: =>A) = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
  }

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
        case Left(a) => Left(a)
        case Right(a) => f(a)
      }
    }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]) = f(value)
}
