package ch03

import scala.annotation.tailrec
import scala.collection.immutable.List

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
			case Nil =>throw new NoSuchElementException("tail of empty list")
			case Cons(head, other) => other
		}
	}

	def head[A](as: List[A]) = {
		as match {
			case Nil => throw new NoSuchElementException("head of empty list")
			case Cons(head, other) => head
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

	/*
		EXERCISE 4: Implement dropWhile, which removes elements from the List prefix as long as they match a predicate. Again, notice these functions take
		time proportional only to the number of elements being dropped—we do not need
		to make a copy of the entire .List
	*/
	def dropWhile[A](as:List[A])(p: A => Boolean): List[A] = {
		@tailrec def loop(as: List[A]): List[A] = {
			if (isEmpty(as) || !p(head(as))) as
			else loop(tail(as))
		}
		loop(as)
	}

	// def dropWhile[A](as:List[A], p: A => Boolean): List[A] = {
	// writint with the upper can not utilize Type reference
	//dropWhile(List(1 ,2 , 4, 5, 6), (x: Int) => x > 3)
	/*
		The main reason  for grouping the arguments this way is to assist with type inference. If
		we do this, Scala can determine the type of without any annotation, based on what it knows about the type of the , which makes theList
	*/
	dropWhile(List(1 ,2 , 4, 5, 6))(x => x > 3)

	/* EXERCISE 5: Using the same idea,  implement the function for setHead replacing the first element of a with a different value */
	def setHead[A](as: List[A], anotherHead: A): List[A] = {
		if (isEmpty(as)) as
		else Cons(anotherHead, tail(as))
	}


}
