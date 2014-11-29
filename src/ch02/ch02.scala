import scala.annotation.tailrec

/**
 * Created by allen on 14-11-13.
 */
object ch02 extends App {
	//  Tail Call Optimizatio1n
	//  is applied when there is no additional work left to do after the recursive call returns

	// Sample 1
	// A call is said to be in 'tail position' if the caller does nothing other than return the value of the recursive call
	// 尾调用就是说一个函数除了 返回递归调用的值以外 没有作其它的事情了
	def factorial(n: Int) = {
		@tailrec def go(n: Int, acc: Int): Int = {
			if (n <=0) acc
			else go(n-1, acc * n)
		}
		go(n, 1)
	}

	// binary search
	// find the position of given number in the Array , -1 represent none
	def arraySort(arr: Array[Int]) = {
	}

	def binarySearch(ds: Array[Int], key: Int): Int = {
		@annotation.tailrec
		def go(low: Int, mid: Int, high: Int): Int = {
			if (low > high) -mid - 1
			else {
				val mid2 = (low + high) / 2
				val d = ds(mid2)
				if (d == key) mid2
				else if (d > key) go(low, mid2, mid2-1)
				else go(mid2 + 1, mid2, high)
			}
		}
		go(0, 0, ds.length - 1)
	}
	val keyTo = binarySearch(Array(1,34, 56, 89,23, 56, 67, 65), 2)
	println("xxxxxxxxxx")
	println(keyTo)
}
