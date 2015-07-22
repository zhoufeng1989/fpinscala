package part2

object Fib {
   def fib(n: Int): Int = {
    def go(n: Int, a: Int, b: Int): Int = {
      if (n == 0) a
      else go(n - 1, b, a + b)
    }
    go(n, 0, 1)
  }                                               //> fib: (n: Int)Int
  fib(0)                                          //> res0: Int = 0
  fib(1)                                          //> res1: Int = 1
  fib(2)                                          //> res2: Int = 1
  fib(3)                                          //> res3: Int = 2
  fib(4)                                          //> res4: Int = 3
  fib(5)                                          //> res5: Int = 5
  fib(6)                                          //> res6: Int = 8
  fib(7)                                          //> res7: Int = 13
}