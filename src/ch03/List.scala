package ch03

import scala.annotation.tailrec

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
	def tail[A](as: List[A]) = {
		as match {
			case Nil =>Nil
			case Cons(head, other) => other
		}
	}

	def isEmpty[A](as: List[A]) = as == Nil
	// EXERCISE 3: Generalize to the function drop, which removes the first  n elements from a list.
	def drop[A](as: List[A], n: Int) = {
		@tailrec def go(as: List[A], n: Int): List[A] = {
			if (isEmpty(as)) Nil
			else if (n == 0) as
			else go(tail(as), n-1)
		}
		go(as, n)
	}
}
