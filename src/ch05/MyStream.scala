package ch05

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Created by allen on 14-12-29.
 */
// Empty 是全局唯一 所以使用 object, case 使之增加了模式匹配的功能
//case object Empty extends MyStream[Nothing] {
//	def uncons = None
//}

/*
Error:(124, 25) `val' parameters may not be call-by-name
case class Cons[+A](hd: => A, tl: => MyStream[A]) extends MyStream[A] {
                        ^
为什么这种写法不行
因为class 在初始化的时候就需要　使用上面的两个参数而不是在引用的时候才初始化

case 默认将所有的constructor参数都变成了val, 而叫名参数是只有在引用的时候才会计算所以产生了冲突
这时标准答案采用了 () => A

case class Cons[+A](hd: () => A, tl: () => MyStream[A]) extends MyStream[A] {
}
*/

trait MyStream[+A] { self =>
	def uncons: Option[(A, MyStream[A])]
	def isEmpty: Boolean = uncons.isEmpty

	/* EXERCISE 1: Write a function to convert a MyStream to a List, which will  force its evaluation and let us look at it in the REPL */
	def toList: List[A] = {
		// private methods are free from the constraints of covariant type position in the contravarint position
		@tailrec def go(un: Option[(A, MyStream[A])], accmulation: List[A]): List[A] = {
			un match {
				case None => accmulation
				case Some((a, myStream)) => go(uncons, a :: accmulation)
			}
		}
		go(uncons, Nil).reverse
	}

	/* EXERCISE 2: Write a function take for returning the first n elements of MyStream */
	def take(n: Int): MyStream[A] = {
		@tailrec def go(un: Option[(A, MyStream[A])], accmulation: MyStream[A], n: Int): MyStream[A] = {
			if (n <=0) accmulation
			else {
				un match {
					case None  => accmulation
					case Some((a, myStream)) => go(uncons, MyStream.cons(a, accmulation), n-1)
				}
			}
		}
		var these = self
		if (n <= 0) self
		else go(these.uncons, MyStream.empty, n)
	}

	/*
		EXERCISE 3:  takeWhile
		set a switch for it, if
	*/
	def takeWhile(p: A => Boolean): MyStream[A] = {
		var these = self
		var accmulation: MyStream[A] = MyStream.empty
		while (!these.isEmpty && p(these.head)) {
			accmulation = MyStream.cons(these.head, accmulation)
			these = these.tail
		}
		accmulation
	}

	def takeWhile2(p: A => Boolean): MyStream[A] = {
		val b = new ListBuffer[A]
		var these = self
		while (!these.isEmpty && p(these.head)) {
			b += these.head
			these = these.tail
		}
		MyStream(b: _*) // 从本质上来讲 两种方式就是进行 Stream的构造只不过 第二种方式显得更优雅
	}

	def head: A = {
		uncons.map {
			case (a, myStream) => a
		}.getOrElse {
			throw new NoSuchElementException("head of empty MyStream")
		}
	}

	def tail: MyStream[A] = uncons.map {
		case (a, myStream) => myStream
	}.getOrElse {
		throw new NoSuchElementException("tail of empty MyStream")
	}

	def stringPrefix = "MyStream"

	// MyStream(1, ?)
	override def toString = {
		stringPrefix + {
			if (isEmpty) "()" else "(" + head + ", ?)"
		}
	}

	def foldRight[B](b: => B)(f: (A, => B) => B):B= {
		self.uncons match {
			case None => b
			case Some((hd, tl)) => f(hd, tl.foldRight(b)(f))
		}
	}

	// not exists[A]
	def exists(predicate: A => Boolean): Boolean = foldRight(false)({
		(a, b) => predicate(a) || b
	})

	/*
		EXERCISE 4: Implement forAll which checks that all elements in the MyStream match a given predicate.
		Your implementation should terminate the traversal as soon as it encounters a non-matching value.
	*/
	def forAll(p: A => Boolean): Boolean = foldRight(true)({
		(a, b) => p(a) && b
	})
}
object MyStream {
	// Construct new empty MyStream
	def empty[A]: MyStream[A] = new MyStream[A] {
		def uncons = None
	}

	def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
		new MyStream[A] {
			lazy val uncons = Some(hd -> tl)
		}
	}

	def apply[A](as: A*): MyStream[A] = {
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))
	}

}
