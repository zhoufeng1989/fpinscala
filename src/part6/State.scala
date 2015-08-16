package part6

/**
 * @author zhoufeng
 */
trait RNG {
  // we return the random number and the new state, leaving the old state unmodified.
  // In effect, we separate the concern of computing what the next state is from the concern
  // of communicating the new state to the rest of the program.
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    } 
  }
  
  def NonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
  
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = NonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }
  
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }
  
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }
  
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }
  
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (Nil, rng)
    case _ => {
      val (i, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      ((i :: xs), r2)
    }
  } 
  
  import part5.Stream.unfold
  
  def intsViaUnfold(count: Int)(rng: RNG): (List[Int], RNG) = {
    def f(pair: (Int, RNG)) = {
      val (count, rng) = pair
      if (count == 0) None
      else {
        val (i, r) = rng.nextInt
        Some((i, r), (count - 1, r))
      }
    }
    val result = unfold((count, rng))(f).toList
    (result map (_._1), result.reverse.head._2)
  }
  
  type Rand[+A] = RNG => (A, RNG)
  
  val int: Rand[Int] = _.nextInt
  
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
   
  def doubleViaMap(rng: RNG): Rand[Double] = 
    map(NonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
    map2(ra, rb)((_, _))
    
  def randIntDouble: Rand[(Int, Double)] = both(int, double)
  
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    rng => {
      fs.foldRight((Nil: List[A], rng))(
          (a, z) => {
            val (i, r) = a(z._2)
            (i :: z._1, r)
          }
     )
    }
    
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(unit(Nil: List[A]))((a, z) => map2(a, z)(_ :: _))
    
  def intsViaSequence(count: Int)(rng: RNG): Rand[List[Int]] = 
    sequence(List.fill(count)(int))  
    
}