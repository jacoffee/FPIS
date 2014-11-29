package ch02

/*
  Created by allen on 14-11-29.
 EXERCISE 3 (hard): Implement and write down a concrete usage partial1 of it. There is only one possible implementation that compiles.

 */
object Excercise3  extends App {
	def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)
}
