package ch05

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Created by allen on 14-12-29.
 */
trait Stream[+A] { self =>
	def uncons: Option[(A, Stream[A])]
	def isEmpty: Boolean = uncons.isEmpty

	/* EXERCISE 1: Write a function to convert a Stream to a List, which will  force its evaluation and let us look at it in the REPL */
	def toList: List[A] = {
		// private methods are free from the constraints of covariant type position in the contravarint position
		@tailrec def go(un: Option[(A, Stream[A])], accmulation: List[A]): List[A] = {
			un match {
				case None => accmulation
				case Some((a, stream)) => go(stream.uncons, a :: accmulation)
			}
		}
		go(uncons, Nil).reverse
	}

	/* EXERCISE 2: Write a function take for returning the first n elements of Stream */
	def take(n: Int): Stream[A] = {
		@tailrec def go(un: Option[(A, Stream[A])], accmulation: Stream[A], n: Int): Stream[A] = {
			if (n <=0) accmulation
			else {
				un match {
					case None  => accmulation
					case Some((a, stream)) => go(stream.uncons, Stream.cons(a, accmulation), n-1)
				}
			}
		}
		var these = self
		if (n <= 0) self
		else go(these.uncons, Stream.empty, n)
	}

	/*
		EXERCISE 3:  takeWhile
		set a switch for it, if
	*/
	def takeWhile(p: A => Boolean): Stream[A] = {
		var these = self
		var accmulation: Stream[A] = Stream.empty
		while (!these.isEmpty && p(these.head)) {
			accmulation = Stream.cons(these.head, accmulation)
			these = these.tail
		}
		accmulation
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

	def head: A = {
		uncons.map {
			case (a, stream) => a
		}.getOrElse {
			throw new NoSuchElementException("head of empty Stream")
		}
	}

	def tail: Stream[A] = uncons.map {
		case (a, stream) => stream
	}.getOrElse {
		throw new NoSuchElementException("tail of empty Stream")
	}

	def stringPrefix = "Stream"

	// Stream(1, ?)
	override def toString = {
		stringPrefix + {
			if (isEmpty) "()" else "(" + head + ", ?)"
		}
	}
}

object Empty extends Stream[Nothing] {
	def uncons = None
}

object Stream {
	// Construct new empty Stream
	def empty[A]: Stream[A] = new Stream[A] {
		def uncons = None
	}

	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		new Stream[A] {
			println(" times ")
			lazy val	 uncons = Some(hd -> tl)
		}
	}

	def apply[A](as: A*): Stream[A] = {
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))
	}

}
