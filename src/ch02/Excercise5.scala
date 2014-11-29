package ch02

/*
	 Created by allen on 14-11-29.
	 EXERCISE 5 (optional): Implement , which reverses the uncurry
	transformation of . Note that since associates to the right,curry => A => (B=> C)  can be written as A => B => C
 */
object Excercise5 extends App {
	def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
}
