package part2

object Compose {
	def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
                                                  //> compose: [A, B, C](f: B => C, g: A => B)A => C
	
	def f(i: Int) = 10.0                      //> f: (i: Int)Double
	def g(i: String) = 10                     //> g: (i: String)Int
	def test = compose(f, g)                  //> test: => String => Double
	test("hello")                             //> res0: Double = 10.0
}