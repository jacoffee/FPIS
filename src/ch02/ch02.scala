package ch02

import scala.annotation.tailrec

/**
 * Created by allen on 14-11-13.
 */
object ch02 extends App {
	//  Tail Call Optimization
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
}
