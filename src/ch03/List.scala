package ch03

import scala.annotation.tailrec

/**
 * Created by allen on 14-11-30.
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
// case class ::[+A](head: A, tail: List[A]) extends List[A]

object List {
	def size[A](as: List[A]) = {
		var count = 0
		var these = as
		while (!isEmpty(these)) {
			count = count + 1
			these = tail(these)
		}
		count
	}

	/* simple and rough implementation of sum and product */
	def sum(ins: List[Int]): Int = ins match {
		case Nil => 0
		case Cons(head, tail) => head  + sum(tail)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(head, tail) => head * product(tail)
	}

	/* high-order implementation */
	/*
		Recursion over lists and generalizing to higher-order functions
		problem: 0 || 1.0,  + || *
		solution:
			Whenever you encounter duplication like this, as we've discussed before,
			you can generalize it away by pulling subexpressions out into function arguments
			任何时候，你碰到了这样的重复。你可以把子表达式抽到一个函数中并且将它们当作函数参数
			head  sum =>  f: (A, B) => B
	*/
	def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
		as match {
			case Nil => z
			case Cons(head, tail) => f(head, foldRight(tail, z)(f)) // 1 :: Nil -> f(1, foldRight(Nil, 0)f)
		}
	}

	/* EXERCISE 10: foldRight is not tail-recursive and will StackOverflow for large lists. Use foldLeft instead */
	def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
		l match {
			case Nil => z // f(z, head)
			case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
			// cause writing in this way, the f will be invoked in the last invocation
			// but  tail has reduced the List to One
			// Cons(1, Cons(2, Cons(3, Nil)))
			// foldLeft(Cons(2, Cons(3, Nil)), 0)(_+ _)
			// foldLeft(Cons(3, Nil), 0)(_+ _)
		}
	}

	def newSum(as: List[Int]) = foldRight(as, 0)(_ + _)
	def newProduct(ds: List[Double]) = foldRight(ds, 1.0)(_ + _)
	// Invocation Trace
	/*
		foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
		f(1, foldRight(Cons(2, Cons(3, Nil)), 0)(_ + _)
	*/

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

	/*
		EXERCISE 8: See what happens when you pass Nil and Cons themselves to foldRight , like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) .
		What do you think this says about the relationship between foldRight and the data constructors of List ?
		1 :: 2:: 3 :: Nil
		equals to apply method
	*/

	/* EXERCISE 9: Compute the length of a list using foldRight. */
	def length[A](l: List[A]): Int = foldRight(l, 0){ (_, acc) => acc + 1 }

	// EXERCISE 11: Write sum, product  and a function to compute the length of  a list using foldLeft
	def sum2(ins: List[Int]): Int = foldLeft(ins, 0)(_ + _)
	def product2(ins: List[Int]): Int = foldLeft(ins, 1)(_ * _)

	def length2[A](ins: List[A]): Int = foldLeft(ins, 0)((acc, _) => acc +1)

	// EXERCISE 12: Write a function that returns the reverse of a list. See if you can write it using a fold
	// Cons(1, Cons(2, Cons(3, Nil)))
	def reverse[A](ins: List[A]): List[A] = foldLeft(ins, Nil: List[A])((B ,A) => Cons(A, B))

	// EXERCISE 14: Implement append in terms of either foldLeft or foldRight
	def append[A](ins: List[A], a: A) = reverse(Cons(a, reverse(ins)))
	def append1[A](ins: List[A], a: A) = foldRight(ins, Cons(a, Nil))((each: A, b: List[A]) => Cons(each, b))

	// EXERCISE 13 (hard): Can you write foldLeft in terms of foldRight? How about the other way around?
	// def foldLeft = foldRight
	// def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
	def foldLeft1[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
		foldRight(reverse(l), z) { (a: A, b: B) => f(b, a) }
	}

	// EXERCISE 15 (hard): Write a function that concatenates a list of lists into a single list. Its runtime should be linear in the total length of all lists
	def flatten[A](ins: List[List[A]]): List[A] = {
		@tailrec def go[A](acc: List[A], each: List[A]): List[A] = {
			each match {
				case Nil => acc
				case Cons(head, tail) => go(append(acc, head), tail)
			}
		}
		ins match {
			case Cons(head, tail) => go(head, flatten(tail))
			case _ => Nil
		}
	}

	// EXERCISE 16: Write a function that transforms a list of integers by adding 1 to each element.
	// (Reminder: this should be a pure function that returns a new List!)
	// ****************
	def map[A,B](l: List[A])(f: A => B): List[B] = {
		val cc = (a:A, b: B) => Cons(f(a), Nil)
		// def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
		// foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
		// foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))
		foldRight(l, Nil:List[B]) { (each, acc) => Cons(f(each), acc) } // currying will do automatic type inference, so the each, acc type will be automatically infered
	}

	// EXERCISE 19: Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to remote all odd numbers from a List[Int]
	def filter[A](l: List[A])(f: A => Boolean): List[A]= {
		foldRight(l, Nil:List[A]) { (each, acc) =>
			if (f(each)) Cons(each, acc) else acc
		}
	}

	// def flatMap[A,B](l: List[A])(f: A => List[B]): List[B]
	// For instance flatMap(List(1,2,3))(i => List(i,i))  should result in List(1,1,2,2,3,3)
	// EXERCISE 20: Write a function flatMap, that works like map except that the function given will return a list instead of a single result, and that list should be
	// inserted into the final resulting list
	def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = flatten(map[A, List[B]](l)(f(_)))

	// EXERCISE 21: Can you use flatMap to implement  filter ?
	def filter1[A](l: List[A])(f: A => Boolean): List[A]= flatMap[A, A](l) { a => { if (f(a)) Cons(a, Nil) else Nil } }

	// EXERCISE 22: Write a function that accepts two lists and constructs a new list
	// by adding corresponding elements. For example, List(1,2,3)  and List(4,5,6) becomes List(5,7,9).
	// ****************
	def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
		(a, b) match {
			case (Nil, _) => Nil  // exit
			case (_, Nil) => Nil
			case (Cons(h, t), Cons(h1, t1)) => Cons(f(h, h1), zipWith(t, t1)(f))
		}
	}

	// EXERCISE 24 (hard): As an example, implement hasSubsequence for
	 // checking whether a contains another as a subsequence. For instance,List List
	 // List(1,2,3,4)  would have List(1,2) List(2,3) List(4)
	def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = zipWith(l, sub)((_, _)) == flatMap(sub)(i => List((i,i)))

}
