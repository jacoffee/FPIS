package ch03

/**
 * Created by allen on 14-11-30.
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ins: List[Int]): Int = ins match {
		case Nil => 0
		case Cons(head, tail) => head  + sum(tail)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	def apply[A](as: A*): List[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	// Excersie2 Implement the function for "removing" the first element of a List  what if the List is Empty
}
