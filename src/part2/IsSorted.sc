package part2

object IsSorted {
	def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
		if (as.length <= 1) true
		else if (ordered(as(0), as(1))) isSorted(as drop 1, ordered)
		else false
	}                                         //> isSorted: [A](as: Array[A], ordered: (A, A) => Boolean)Boolean
	def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
		val length = as.length
		def go(i: Int): Boolean = {
			if (i >= length - 1) true
			else if (ordered(as(i), as(i + 1))) go(i + 1)
			else false
		}
		go(0)
	}                                         //> isSorted2: [A](as: Array[A], ordered: (A, A) => Boolean)Boolean
	val arr = Array(1, 2, 3, 4)               //> arr  : Array[Int] = Array(1, 2, 3, 4)
	isSorted[Int](arr, (i, j) => (i <= j))    //> res0: Boolean = true
	isSorted2[Int](arr, (i, j) => (i <= j))   //> res1: Boolean = true
	val arr1 = Array(4, 3, 2, 1)              //> arr1  : Array[Int] = Array(4, 3, 2, 1)
	isSorted[Int](arr1, (i, j) => (i <= j))   //> res2: Boolean = false
	isSorted2[Int](arr1, (i, j) => (i <= j))  //> res3: Boolean = false
}