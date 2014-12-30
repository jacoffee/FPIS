package ch05

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import Stream._
/**
 * Created by allen on 14-12-29.
 */

// ADT
// Empty 是全局唯一 所以使用 object, case 使之增加了模式匹配的功能
case object Empty extends Stream[Nothing]

/*
Error:(124, 25) `val' parameters may not be call-by-name
case class Cons[+A](hd: => A, tl: => Stream[A]) extends Stream[A] {
                        ^
为什么这种写法不行
因为class 在初始化的时候就需要　使用上面的两个参数而不是在引用的时候才初始化

case 默认将所有的constructor参数都变成了val, 而叫名参数是只有在引用的时候才会计算所以产生了冲突
这时标准答案采用了 () => A

case class Cons[+A](hd: () => A, tl: () => Stream[A]) extends Stream[A] {
}
*/
case class Cons[+A](hd: () => A, tl: () => Stream[A]) extends Stream[A]

trait Stream[+A] { self =>
	def isEmpty = self match {
		case Empty => true
		case Cons(_, _) => false
	}

	/* EXERCISE 1: Write a function to convert a Stream to a List, which will  force its evaluation and let us look at it in the REPL */
	def toList: List[A] = {
		@tailrec def go(s: Stream[A], acc: List[A]): List[A] = s match {
			case Cons(hd, tl) => go(tl(), hd() :: acc)
			case _ => acc
		}
		go(self, Nil).reverse
	}

	// with ListBuffer && Cons
	def toList1: List[A] = {
		var these = self
		var buffer = new scala.collection.mutable.ListBuffer[A]()
		while (!these.isEmpty) {
			buffer += these.head
			these = these.tail
		}
		buffer.toList
	}

	/* EXERCISE 2: Write a function take for returning the first n elements of Stream */
	/*
		Create a new Stream[A] from taking the n first elements from this. We can achieve that by recursively
		calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
		we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
		at the stream at all.
		Cons(h1, h2, Cons(h3, Empty))

		真是巧妙啊 针对递归 寻找递归的意识是非常重要的
  	*/
	def take(n: Int): Stream[A] = self match {
		case Cons(h, t) if n > 1 => Cons(h, () => t().take(n-1))
		case Cons(h, _) if n == 1 => Cons(h, () => Empty)
		case _ => Empty
	}

	/*
		EXERCISE 3:  takeWhile
		set a switch for it, if
	*/
	def takeWhile(p: A => Boolean): Stream[A] = {
		var these = self
		var accmulation: Stream[A] = Empty
		// Cons(() => 3, () => accmulation)
		while (!these.isEmpty && p(these.head)) {
			val h = these.head // 使用变量接收 破除these.head 的引用
			accmulation = cons(h, Empty) // 嵌套太深
			these = these.tail
		}
		accmulation
		// 此时结果是List(1) 因为整个循环结束之后 Stream 变成了  Stream(1, 5, 1, 5, 10, 42)
		// 由于延迟计算，所以these.head 变成了1
	}

	def takeWhile2(p: A => Boolean): Stream[A] = {
		val b = new ListBuffer[A]
		var these = self
		while (!these.isEmpty && p(these.head)) {
			b += these.head
			these = these.tail
		}
		Stream(b: _*) // 从本质上来讲 两种方式就是进行 Stream的构造只不过 第二种方式显得更优雅
	}

	/*
		EXERCISE 5: Use foldRight to implement takeWhile. This will
		construct a stream incrementally, and only if the values in the result are demanded
		by some other expression
	*/
	def takeWhile3(p: A => Boolean): Stream[A] =
		foldRight(Empty: Stream[A])((a, b) => if(p(a)) Cons(() => a, () => b) else b)

	def head: A =
		self match {
			case Empty => throw new NoSuchElementException("head of empty Stream")
			case Cons(hd, _) => hd()
		}

	def tail: Stream[A] =
		self match {
			case Empty => throw new NoSuchElementException("tail of empty Stream")
			case Cons(_, tl) => tl()
		}

	def stringPrefix = "Stream"

	// Stream(1, ?)
	override def toString = {
		stringPrefix + {
			if (isEmpty) "()" else "(" + head + ", ?)"
		}
	}

	def foldRight[B](b: => B)(f: (A, => B) => B):B= {
		self match {
			case Empty => b
			case Cons(hd, tl) => f(hd(),  tl().foldRight(b)(f))
		}
	}

	// not exists[A]
	def exists(predicate: A => Boolean): Boolean = foldRight(false)({
		(a, b) => predicate(a) || b
	})

	/*
		EXERCISE 4: Implement forAll which checks that all elements in the Stream match a given predicate.
		Your implementation should terminate the traversal as soon as it encounters a non-matching value.
	*/
	def forAll(p: A => Boolean): Boolean = foldRight(true)({
		(a, b) => p(a) && b
	})

	/* EXERCISE 6: Implement map, flatMap, filter and append using  foldRight */
	def map[B >: A](f: A => B): Stream[B] =
		foldRight(Empty: Stream[B])(cons(_, _))
}

object Stream {

	def empty = Empty

	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		// lazy val head = hd
		// lazy val tail = tl
		Cons(() => hd, () => tl)
		// new Stream[A] { lazy val	 uncons = Some(hd -> tl) }
	}

	def apply[A](as: A*): Stream[A] = {
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))
	}
}
