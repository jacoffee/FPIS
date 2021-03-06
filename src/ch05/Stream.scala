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
			case Cons(hd, tl) => f(hd(),  tl().foldRight(b)(f)) // f(hd(), Empty)
		}
	}

	// not exists[A]
	// in the worst suituation, the whole Stream has been tranversed
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
	def map[B >: A](f: A => B): Stream[B] = {
		foldRight(Empty: Stream[B])((a, b) =>cons(f(a), b))
	}

	def appendOne[B >: A](b: B): Stream[B] = cons(b, self)
	def append[B>:A](s: => Stream[B]): Stream[B] =
		foldRight(s)((h,t) => cons(h,t))
	def filter(p: A => Boolean): Stream[A] = {
		foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)
	}

	def flatMap[B >: A](f: A => Stream[B]): Stream[B] = {
		foldRight(Empty: Stream[B])((a, b) => {
			// fetch every element in f(a) and put in b
			@tailrec def go(each: Stream[B], acc: Stream[B]): Stream[B] = {
				each match {
					case Cons(hd, tl) => go(tl(), cons(hd(), acc))
					case Empty => acc
				}
			}
			go(f(a), b)
			//利用ListBuffer 实现排序 不过每一次循环就会产生一个ListBuffer的开销
		})
	}

	def flatMap1[B](f: A => Stream[B]): Stream[B] =
		foldRight(Empty: Stream[B])((h,t) => f(h) append t)


	/*
		EXERCISE 7: Generalize ones slightly to the function constant which
		returns an infinite Stream of a given value
	*/
	def constant[A](a: A): Stream[A] = cons(a, constant(a))

	/*
		EXERCISE 8: Write a function that generates an infinite stream of integers,
		starting from n, then n +1,  n + 2,
		n
		n + 1
		n + 2

	*/
	def from(n: Int): Stream[Int] = {
		/*
			cons(n, from(n+1))
				cons(n+1, from(n+2))
					cons(n + 2, from(n + 3))
		*/
		cons(n, from(n + 1))
	}


	/*
		EXERCISE 9: Write a function fibs that generates the infinite stream of
		Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on
	*/
	def fibs: Stream[Int] = {
		def go(prev: Int, cur: Int): Stream[Int] = {
			cons(prev, go(cur, cur + prev))
		}
		// the first two calculations seem to be conflicting with the definition cause 1 is the next of 0
		go(0, 1) // cons(0, go(1, 0))  cons(0, cons(1, cons(1, go(2, 1))))
	}

	/*
		EXERCISE 10: We can write a more general stream building function. It takes
		an initial state, and a function for producing both the next state and the next value
		in the generated stream. It is usually called :unfold

		Option is used to indicate when the should be terminated, if at all. The unfold
	function is the most general Stream-building function
	*/
	// def uncons: Option[(A, Stream[A])]
	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
		f(z) match {
			case Some((a, s)) => cons(a, unfold(s)(f))  // Some之中就是处理原来的逻辑 只不过将其抽象化了
			case _ => Empty
		}
	}
	/*
		EXERCISE 11: Write fibs, from, constant ones, and in terms of unfold.
	*/
	def constantViaFold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n))
	//  def uncons: Option[(A, Stream[A])]
	// ?? n如何自己增长
	def fromViaFold(initial: Int): Stream[Int] = unfold(initial)(s => Some(s, s +1))

	def fibsViaFold: Stream[Int] = {
		unfold((0, 1))(s =>
			Some(s._1, (s._2, s._1 + s._2))
		)
	}

	/*
	EXERCISE 12: Use to implement zip map take takeWhile
	 The function should continue the traversal aszipAll
	 zipAll
	long as either stream has more elements — it uses to indicate Option whether
	each stream has been exhausted.
	*/
	def mapViaFold[B >: A](f: A => B): Stream[B] = {
		unfold(self)(s => {
			if (s.isEmpty) None
			else Some(f(s.head) -> s.tail)
		}) // 没有出口 会一直tail的
	}
	def takeViaFold(n: Int): Stream[A] = {
		unfold((self, n)){
			case (Cons(h, t), n) if n >1=> Some(h() -> (t(), n-1))
			case (Cons(h, _), n) if n == 1=> Some(h() -> (Empty, n))
			case _ => None
		}
	}

	def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = {
		// change main invocator every time the tranversal carries out
		(self, b) match {
			case (Cons(h1, t1), Cons(h2, t2)) => cons(f(h1(),h2()), t1().zipWith(t2())(f))
			case (Empty, _) => Empty
			case (_, Empty) => Empty
		}
	}

	def zipWithViaFold[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = {
		unfold((self,b))(
			_ match {
				case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(),h2()) -> (t1(), t2()))
				case (Empty, _) => None
				case (_, Empty) => None
			}
		)
	}

	// 作为调用者 你需要处理当 List(1,2,3,4) zipWithAll List(1,2,3)  前面这个多出来的应该怎样处理
	def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = {
		unfold((self, s2))(
			_ match {
				case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
				case (Empty, Cons(h2, t2)) => {
					Some(f(None, Some(h2())) -> (Empty, t2()))
				}
				case (Cons(h1, t1), Empty) => {
					Some(f(Some(h1()), None) -> (t1(), Empty))
				}
				case (Empty, Empty) => None
			}
		)
	}

//	def zipAll[B >: A, C >: A](b: Stream[B])(f: (A, B) => C): Stream[C] = {
//		// 上面的Option 数据结构成功解决了 此处我需要通过 >: 来解决类型兼容的问题 如果不存在的话就直接使用None表示
//		(self, b) match {
//			case (Cons(h1, t1), Cons(h2, t2)) => cons(f(h1(),h2()), t1().zipWith(t2())(f))
//			case (Empty, Cons(h2, t2)) => Empty
//			case (Cons(h1, t1), Empty) => Empty
//			case (Empty, Empty) => Empty
//		}
//		Empty
//	}

	def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
		zipWithAll(s2)((_, _))

	//def zipAll[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = {
	/*
		Using lazy lists, can you see
		how you could implement hasSubsequence by combining some other
		functions we have already written?
	*/
	// Stream(1, 2, 3, 4, Empty) --> Stream(1, 2, 3, Empty) -->Stream(2, 3, Empty)
	def hasSubsequence[B >: A](b: Stream[B]): Boolean = tails.exists(_.startsWith(b))
	/*
  		`s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness, we can compose these three separate logical steps--
  		the zipping, the termination when the second stream is exhausted, and the termination if a nonmatching element is found or the first stream is exhausted.
  	*/
	//  Note: any two values, , and ,x y can be compared for equality in Scala using the expression .
	def startsWith[A](s2: Stream[A]): Boolean = {
		// Stream(1,2,3, Empty) -- Stream(1, 2, Empty)
		// Stream((Some(1), Some(1)), (Some(2), Some(2), (Some(3), None))
		zipAll(s2).takeWhile3(!_._2.isEmpty).forAll {
			case (h1, h2) => h1 == h2
		}
	}

	/*
		EXERCISE 14: implement tails using unfold. For a given Stream,
		tails returns the of suffixes of the input sequence, starting with the
		original Stream. So, given Stream(1,2,3) , it would return
		Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream.empty) .
	*/
	def tails: Stream[Stream[A]] =
		unfold(self)(s =>
			if (s.isEmpty) None // don't forget to add the switch
			else Some(s, s.tail)
		)

	def ===[B >: A](b: Stream[B]): Boolean = {
		// 1 2   zip 1 2  3
		// (Some(1), Some(1)) (Some(2), Some(2)) (None, Some(3))
		// (Some(1), Some(1)) (Some(2), Some(2))
		val zipped = zipAll(b)
		if (zipped.exists(r => r._1.isEmpty ||  r._2.isEmpty)) false
		else zipped.forAll {
			case (h1, h2) => h1 == h2
		}
	}


	/*
		EXERCISE 15 (hard, optional): Generalize tails to the function
		scanRight, which is like a foldRight that returns a stream of the
		intermediate results. For example:

		Stream(1,2,3).scanRight(0)(_ + _).toList

		List(1+2+3+0, 2+3+0, 3+0, 0)
	*/
	def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
		var these = self
		var buffer =  ListBuffer[B]()
		while(!these.isEmpty) {
			buffer +=  these.foldRight(z)(f)
			these = these.tail
		}
		buffer += z
		Stream(buffer: _*)
	}


	def unfold3[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
		f(z) match {
			case Some((a, s)) => cons(a, unfold3(s)(f))  // Some之中就是处理原来的逻辑 只不过将其抽象化了
			case _ => Empty
		}
	}

	/*
  	The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right.
  	It can be implemented using `foldRight` though.

  	The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results,
  	which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.

  	// p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
  */
	def scanRightViaUnfold[B](z: => B)(f: (A, => B) => B): Stream[B] = {
		// z 无法很好的放到最后面去
		unfold(self)(s => {
			if (s.isEmpty) None
			else Some(s.foldRight(z)(f), s.tail)
		})
	}

	def scanRightViaFoldRight[B](z: => B)(rightF: (A, => B) => B): Stream[B] = {
		//def foldRight[B](b: => B)(f: (A, => B) => B):B= {
		// List(1)
		// List(1+2+3+0, 2+3+0, 3+0, 0)
		// (3, (0, Stream(0))
		foldRight(z -> Stream(z))( (a, b) => {
			// println(" a: "+ a + " p0 " + b._1)
			val r = rightF(a,  b._1)
			(r, cons(r, b._2))
		})._2

		foldRight(z -> Stream(z))( (a, b) => {
			lazy val po = b
			val r = rightF(a,  po._1)
			(r, cons(r, po._2))
		})._2
		// Stream(1, 2, 3)
		/*
			Stream(1, 2)

			foldRight(0-> Stream(0))(f)
			Empty.foldRight(0-> Stream(0))(f)

			step4
			f(
				1, {
					f(
						2,  0-> Stream(0)   ====>
					)
				}
			)

			step5
			f(
				1,
				{
					println(" a: "+ 2 + " p0 " + 0)
					val r = rightF(2, 0)
					(r, cons(r,  Stream(0)))
				}
			)

			step6

			println(" a: "+ 1 + " p0 " + {
					(
						println(" a: "+ 2 + " p0 " + {
							println(" a: "+ 3 + " p0 " + 0)
							val r = rightF(3, 0)
							(r, cons(r, Stream(0)))
						}
					)
				}._1),
			(
				println(" a: "+ 1 + " p0 " + {
					println(" a: "+ 2 + " p0 " + 0)
					val r = rightF(2, 0)
					(r, cons(r, Stream(0)))
				}._1),
				val r = rightF(1, {
					println(" a: "+ 2 + " p0 " + 0)
					val r = rightF(2, 0)
					(r, cons(r, Stream(0)))
				}._1
				(r, cons(r, {
					println(" a: "+ 2 + " p0 " + 0)
					val r = rightF(2, 0)
					(r, cons(r, Stream(0)))
				}._2))
			)._2


			取出tuple的值时候, 所有的值都会同步计算
			val tupleTest = ({println("number1 "); 1}, {println("number2"); 3})
				println(" tupleTest " + tupleTest._2)
				/*
					number1
					number2
					tupleTest 3
				*/
			A(1, Stream(2,3).foldRight(z)(A))
				A(1, A(2, Stream(3).foldRight(z)(A)))
					A(1, A(2, A(3, Empty.foldRight(z)(A)))
						A(1, A(2, A(3, b))
							A(1, A(2, r1))
								A(1, r2)
							r3
		*/

		/*
			Stream(1,2,3)
			 a:  3 p0 0
			 a:  2 p0 3
			 a:  1 p0 5
			 a:  3 p0 0
			 a:  2 p0 3
			 a:  3 p0 0
		 */
	}

	def foldRight11[B](b: => B)(f: (A, => B) => B):B= {
		self match {
			case Empty => b
			case Cons(hd, tl) => f(hd(),  tl().foldRight(b)(f)) // f(hd(), Empty)
		}
	}
}


object Stream {

	def empty = Empty

	def unit[A](a: => A) = cons(a, empty)

	// 1 Empty
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
