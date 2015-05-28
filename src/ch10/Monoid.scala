package ch10

/**
 * Created by allen on 5/26/15.
 */

trait Monoid[A] {
	def op(a1: A, a2: A): A
	def zero: A // identity element
}

object MyMonoid extends App {
	// val listMonoid[A] are not allowed
	def listMonoid[A] = new Monoid[List[A]] {
		def op(a1: List[A], a2: List[A]): List[A] = a1 ::: a2
		def zero: List[A] = Nil
	}


	/* EXERCISE 1: Give Monoid instances for integer addition and multiplication as well as the Boolean operators. */

	val intAddition: Monoid[Int] = new Monoid[Int] {
		def op(a1: Int, a2: Int): Int = a1 + a2
		def zero: Int = 0
	}

	val intMultiplication: Monoid[Int] = new Monoid[Int] {
		def op(a1: Int, a2: Int): Int = a1 * a2
		def zero: Int = 1
	}

	// 针对 monoid[type] 这个方法的话 以上面两个int为例， 隐式参数寻找是无法确认哪一个的
	// 这个时候我们需要进行数据结构的进一步 抽象 Wrapper

	// for intAddition
	case class Product(value: Int)

	implicit val productMonoid: Monoid[Product] = new Monoid[Product] {
		def op(a: Product, b: Product) = Product(a.value * b.value)
		def zero = Product(1)
	}

	// for multiplication
	case class Sum(value: Int)

	implicit val sumMonoid: Monoid[Sum] = new Monoid[Sum] {
		override def op(a1: Sum, a2: Sum): Sum = Sum(a1.value + a2.value)
		override def zero: Sum = Sum(0)
	}

	val pMonoid = monoid[Product]
	val sMonoid = monoid[Sum]

	val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
		def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
		def zero: Boolean = true
	}

	val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
		def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
		def zero: Boolean = false
	}

	/* EXERCISE 2: Give a Monoid instance for combining Options: */
	def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
		override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
		def zero: Option[A] = None
	}

	/*
		EXERCISE 3: A function having the same argument and return type is called an endofunction
		自函数
	*/
	def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
		def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
		def zero: (A) => A = identity
	}

	def dual[A](A: Monoid[A]) =
		new Monoid[A] {
			def op(a1: A, a2: A): A = A.op(a2, a1)
			override def zero: A = A.zero
		}


	// foldRight
	val words = List("Hic ", " Est ", " Index ")
	/* Excercise5 */
	def wordMonoid(s: String): Monoid[String] = new Monoid[String] {
		// 以空格结尾  以空格开头
		val spaceHead = """^\s+.*""".r
		val spaceEnd = """.*\s+$""".r

		def op(a1: String, a2: String): String = {
			val withSpaceEnd_? = spaceEnd.findFirstMatchIn(a1).isDefined
			val withSpaceHead_? = spaceHead.findFirstMatchIn(a2).isDefined

			a1 + { if (!withSpaceEnd_? && !withSpaceHead_?) " " else  "" } + a2.trim
		}
		def zero: String = s
	}

	val wordMonoidInstance = wordMonoid("")
	println(words.foldRight(wordMonoidInstance.zero)(wordMonoidInstance.op))

	/* EXERCISE 6: Implement concatenate, a function that folds a list with a monoid: */
	def concatenate[A](as: List[A], m: Monoid[A]): A =
		as.foldLeft(m.zero)(m.op)

	/*
		EXERCISE 6

		But what if our list has an element type that doesn't have a Monoid instance? Well,
		we can always map over the list to turn it into a type that does.
	*/
	def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
		as.foldRight(m.zero)({
			(value, acc) => m.op(acc, f(value))
		})
	}

	/*
		EXERCISE 8 (hard): The foldMap function can be implemented using either foldLeft or foldRight.
		But you can also write foldLeft and foldRight using foldMap! Try it.
	*/
	def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
		val f1 = f.curried
		// B -> 先返回B -> B 然后调用 (B -> B)
		foldMap(as, endoMonoid[B])(f.curried)(z)
	}

	def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
		foldMap(as, endoMonoid[B])(a => f(_, a))(z)
	}


	sealed trait WC
	case class Stub(chars: String) extends WC
	case class Part(lstub: String, count: Int, rstub: String) extends WC

	val wcMonoid: Monoid[WC] = new Monoid[WC] {
		// 主要是考虑如何将两个WC 类型的合并
		// Stub Part | Stub Stub | Part Stub |  Part Part
		def op(a1: WC, a2: WC): WC = (a1, a2) match {
			case (Stub(x), Stub(y)) => Stub(x + y)
			case (Stub(x), Part(l, c, r)) => Part(x + l, c ,r)
			case (Part(l, c, r), Stub(x)) => Part(l, c, r + x)
			case (Part(l1, c1, r1), Part(l2, c2, r2)) => Part(l1, c1 + c2 + ({if ((r1 + l2).isEmpty) 0 else 1 }), r2)
		}
		def zero: WC = Stub("")
	}

	/*
		EXERCISE 10: Use the WC monoid to implement a function that counts words in a String by recursively splitting it into substrings
		and counting the words in those substrings.

		将字符串不断拆分子串 并且统计数量
		"dying in the sun"
		"dying in the"
		"dying in"
		"dying"
	*/

	/*
		why splitting

		cause foldleft to string could be a wasting operation for the string concatnation

		EXERCISE 11: Implement an efficient foldMap for IndexedSeq, a common supertype
		for various data structures that provide efficient random access.

		EXERCISE 12: Use foldMap to detect whether a given IndexedSeq[Int] is ordered.
		You will need to come up with a creative Monoid instance.
	*/

	def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
		v.foldLeft(m.zero)({
			(mb, a) => m.op(mb, f(a))
		})

	type IntB = (Int, Boolean)
	val biggerThanMonoid = new Monoid[IntB] {
		def op(a1: IntB, a2: IntB): IntB = (a1, a2) match {
			case ((v1, b1), (v2,b2)) => if (v1 > v2) (v1, true) else (v2, false)
		}

		def zero: IntB = (0, true)
	}

	val lessThanMonoid = new Monoid[IntB] {
		def op(a1: IntB, a2: IntB): IntB = (a1, a2) match {
			case ((v1, b1), (v2,b2)) => if (v1 < v2) (v1, true) else (v2, false)
		}

		def zero: IntB = (0, true)
	}


	def descend_?(v: IndexedSeq[Int]) =
		foldMapV(v, biggerThanMonoid)(a => (a, true))._2

	object OptionFoldable extends Foldable[Option] {
		override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
			case Some(a) => mb.op(mb.zero, f(a))
			case None => mb.zero
		}

		override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
			case Some(a) => f(z, a)
			case None =>  z
		}

		override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
			case Some(a) => f(a, z)
			case None => z
		}

		override def toList[A](as: Option[A]): List[A] =
			foldRight(as)(Nil: List[A])(_ :: _)
	}

	// monoid compose
	/* This means, for example, that if types A and B are monoids, then the tuple type (A, B) is also a monoid */
	def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] =
		new Monoid[(A, B)] {
			override def op(a1: (A, B), a2: (A, B)): (A, B) =
				(A.op(a1._1,a2._1), B.op(a1._2, a2._2))

			override def zero: (A, B) = (A.zero, B.zero)
		}

	productMonoid(intAddition, intAddition)

	// 原来的类型 A  Monoid 类型 (A, ) 所以需要使用f 将其转换成 (A, A)
	foldMap(List(1, 2, 3, 4), productMonoid(intAddition, intAddition))(a => (1, a))

	def coproductMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[Either[A,B]] =
		new Monoid[Either[A, B]] {
			override def op(a1: Either[A, B], a2: Either[A, B]): Either[A, B] = (a1, a2) match {
				case (l @ Left(v1), Left(v2)) => l
				case (l @ Left(v1), Right(v2)) => l
				case (Right(v1), l @ Left(v2)) => l
			}

			override def zero: Either[A, B] = Left(A.zero)
		}

	/* EXERCISE 19: Write a monoid instance for functions whose results are monoids. */
	def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = {
		new Monoid[A => B] {
			override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))
			override def zero: A => B = _ => B.zero
		}
	}

	/*
		EXERCISE 20: Use monoids to compute a frequency map of words in an IndexedSeq of Strings.
	*/
	val mapStringMonoid = new Monoid[Map[String, Int]] {

		override def op(a1: Map[String, Int], a2: Map[String, Int]): Map[String, Int] =
			a1 ++ a2.map { case (k,v) => k -> (v + a1.getOrElse(k, 0)) }
		override def zero: Map[String, Int] = Map.empty[String, Int]
	}

	def frequencyMap(strings: IndexedSeq[String]): Map[String, Int] = {
		strings.foldRight(mapStringMonoid.zero)((v, acc) => {
			mapStringMonoid.op(Map(v -> 1), acc)
		})
	}

	val wordList = Array("word", "yes", "no", "yes", "word")

	println(frequencyMap(wordList))

	implicit val stringMonoid: Monoid[String] = new Monoid[String] {
		def op(a1: String, a2: String): String = a1 + a2
		def zero: String = ""
	}

	def monoid[A](implicit A: Monoid[A]): Monoid[A] = A
	monoid[String] // the stringMonoid fits the method paramteter like a glove
}

import ch03.Tree
object TreeFoldable extends Foldable[Tree] {

	import MyMonoid._
	import Tree._

	override def foldRight[A,B](as: Tree[A])(z: B)(f: (A, B) => B): B =
		foldMap(as)(f.curried)(endoMonoid[B])(z)

	override def foldLeft[A,B](as: Tree[A])(z: B)(f: (B, A) => B): B =
		foldRight(as)(z)((b, a) => f(a, b))

	override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
		fold(as)(f)(mb.op)

	// fetch what's in the Tree and put it into List
	// super 也是使用，so you need explicit invocation
	override def toList[A](as: Tree[A]): List[A] = super.toList(as)
}

trait Foldable[F[_]] {

	import MyMonoid._

	def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
		foldMap(as)(f.curried)(endoMonoid[B])(z)

	def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
		foldRight(as)(z)((a, b) => f(b, a))

	def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
		foldRight(as)(mb.zero)((a, b) => f(a))

	def toList[A](as: F[A]): List[A] =
		foldRight(as)(Nil: List[A])({(a, b) => a :: b })
}




abstract class OrderMonoid[A : Numeric] {
	import Numeric._

	type AB = (A, Boolean)

	val init: AB = (implicitly[Numeric[A]].zero, true)

	val biggerThanMonoid = new Monoid[AB] {
		def op(a1: AB, a2: AB) =
			(a1, a2) match {
				case ((v1, b1), (v2,b2)) =>
					if (implicitly[Numeric[A]].gt(v1, v2)) (v1, true)
					else (v2, false)
			}

		def zero: AB = init
	}

	val lessThanMonoid = new Monoid[AB] {
		def op(a1: AB, a2: AB): AB =
			(a1, a2) match {
				case ((v1, b1), (v2,b2)) =>
					if (implicitly[Numeric[A]].lt(v1, v2)) (v1, true)
					else (v2, false)
			}

		def zero: AB = init
	}


}