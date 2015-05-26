package ch10

/**
 * Created by allen on 5/26/15.
 */

trait Monoid[A] {
	def op(a1: A, a2: A): A
	def zero: A // identity element
}

object MonoidTest extends App {
	val stringMonoid: Monoid[String] = new Monoid[String] {
		def op(a1: String, a2: String): String = a1 + a2
		def zero: String = ""
	}

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
	def EndoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
		def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2
		def zero: (A) => A = identity
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
	def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
		as.map(f).foldLeft(m.zero)(m.op)

	/*
		EXERCISE 8 (hard): The foldMap function can be implemented using either foldLeft or foldRight.
		But you can also write foldLeft and foldRight using foldMap! Try it.
	*/
	def foldLeftViaMap[A, B](as: List[A], z: B)(f: (B, A) => B): B = ???

	def foldRightViaMap[A, B](as: List[A], z: B)(f: (A, B) => B): B = ???
}
