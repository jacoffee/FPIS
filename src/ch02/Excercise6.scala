package ch02

/*
  Created by allen on 14-11-29.
  EXERCISE 6: Implement the higher-order function that composes two functions
 */
object Excercise6 extends App {
	def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
