package ch02

/*
 	Created by allen on 14-11-29.
  	EXERCISE 4 (hard): Let's look at another example, , which converts a currying
	function of arguments into a function of one argument that returns anotherN
	function as its result. Here again, there is only one implementation that11
	typechecks
 */
object  Excercise4 extends App {
	def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => f(a, _)
}
