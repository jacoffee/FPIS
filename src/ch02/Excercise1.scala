package ch02

import scala.annotation.tailrec
/**
 * Created by allen on 14-11-13.
	EXERCISE 1 (optional):
	Write a function to get the "nth" Fibonacci number. --> n-1 + n-2
	Then first two Fibonacci numbers are 0 and 1, and the next number is always the sum of the previous two.
	Your definition should use a local tail-recursive function.

 */
// 1、1、2、3、5、8、13、21
// F0=0，F1=1，Fn=F(n-1)+F(n-2)（n>=2，n∈N*）

object Excercise1 extends App {
	// Scala compiles the recursion to iterative loops that do not consume call stack frames for each iteration

	// simple and ugly without tail recursion option
	def simpleRecursion(n: Long): Long = {
		 if (n >= 1 && n <= 2) 1
		else simpleRecursion(n-1) + simpleRecursion(n-2)
	}

	// iteration method
	// an = an-1 + an-2
	// next round the an-1 <- an; an-2<- an-1
	def iteration(n: Int): Long = { // 1000 -> 60ms
		if (n <= 1) 1
		else {
			var sum: Long = 0L
			var prev = 0L
			var cur = 1L
			for (num <- 2 to n ) {
				sum = prev + cur
				prev = cur
				cur = sum
			}
			sum
		}
	}

	def tailRecursion(n: Long): Long = { // 1000 -> 51
		@tailrec def loop(n: Long, prev: Long, cur: Long): Long = {
			if (n < 1) prev
			else loop(n-1, cur, cur+prev)
			// 由于函数式编程 强调输入 | 输出， 所以每次完成两数之后
			// 之前的后一个 变成了 下次计算的 prev
			// 而前一次的 则变成了 下一次的当前值
		}
		loop(n, 0, 1)
	}

	// compare efficiency
	val start  = System.currentTimeMillis
	val r1 = tailRecursion(1000)
	println(" time consuming " + (System.currentTimeMillis -start))
	println(" result " + r1)
	//Integer.MAX_VALUE
	//Long.MaxValue
}
