package part2

object Curry {
	def curry[A, B, C](f: (A, B) => C): A => (B=>C) = a => f(a, _)
                                                  //> curry: [A, B, C](f: (A, B) => C)A => (B => C)
	def uncurry[A, B, C](f: A => B => C): (A, B) => C = f(_)(_)
                                                  //> uncurry: [A, B, C](f: A => (B => C))(A, B) => C
	def test(i: Int, j: Double) = "hello"     //> test: (i: Int, j: Double)String
	val x = curry(test)                       //> x  : Int => (Double => String) = <function1>
	x(10)                                     //> res0: Double => String = <function1>
	x(10)(20.0)                               //> res1: String = hello
	val x1 = uncurry(x)                       //> x1  : (Int, Double) => String = <function2>
	x1                                        //> res2: (Int, Double) => String = <function2>
	val y = (test _).curried                  //> y  : Int => (Double => String) = <function1>
	y(10)                                     //> res3: Double => String = <function1>
	y(10)(20.0)                               //> res4: String = hello
	uncurry(y)                                //> res5: (Int, Double) => String = <function2>
}