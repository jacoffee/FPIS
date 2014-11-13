package ch02

/**
 * Created by allen on 14-11-13.
	EXERCISE 1 (optional):
	Write a function to get the "nth" Fibonacci number. --> n-1 + n-2
	Then first two Fibonacci numbers are 0 and 1, and the next number is always the sum of the previous two.
	Your definition should use a local tail-recursive function.

 */

object Excercise1 extends App {
	// // Scala compiles the recursion to iterative loops that do not consume call stack frames for each iteration

	// simple and ugly without tail recursion option
	// n should be bigger than one
	def simpleFibonacci(n: Int): Int = {
		 if (n >= 1 && n <= 2) n -1
		else simpleFibonacci(n-1) + simpleFibonacci(n-2)
	}

	def fib(n: Int): Int = ???
}
