package part8

import part8.Prop.{SuccessCount, FailedCase}
import part6.{RNG, State}
/**
 * Created by zhoufeng on 15-8-26.
 */
object Prop {
  type SuccessCount = Int
  type FailedCase = String
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}


case class Gen[A](sample: State[RNG, A])

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.NonNegativeInt).map{start + _ % (stopExclusive - start)})

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.int).map{x => if(x > 0) true else false})

}
