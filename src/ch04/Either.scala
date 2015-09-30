package ch04

import ch03.{ ::, List, Nil }
import ch03.List.{ exists, foldLeft, find  }

/*
	One thing you may have noticed with is that it doesn't tell us very much about Option
	what went wrong in the case of an exceptional condition
	All it can do is give us None indicating that there is no value to be had. But sometimes we want to know more.
 */

sealed trait Either[+E,  +A] { self =>
	/*
		EXERCISE 7: Implement versions of map, flatMap, orElse and map2 on
		Either that operate on the right values
		Either[E, A] 这样写下面是编译不通过的  因为下面传递的可能是E, A的子类的Left || Right
		这是Either[E, A] 类型所不支持的
	*/
	def map[B](f: A => B): Either[E, B] = {
		self match {
			case Left(e) => Left(e)
			case Right(a) => Right(f(a))
		}
	}

	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
		self match {
			case l @ Left(e) => l
			case Right(a) => f(a)
		}
	}

	def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
		self match {
			case l @ Left(e) => l
			case r @ Right(a) => r
			case _ =>  b
		}
	}
	def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
		(self, b) match {
			case (Right(a), Right(b)) => Right(f(a, b))
			case (_, l2 @ Left(e)) => l2
			case (l1@ Left(e), _) => l1
		}
	}

	def isLeft = self match {
		case l @ Left(e) => true
		case _ => false
	}

	def isRight = !isLeft

	// 如果Either 存在一个Left 则结果就返回那个Left 否则返回List[Either[E, A]]
	// def sequence[A](a: List[Option[A]]): Option[List[A]]
	def sequence[EE >: E, C >: A](a: List[Either[EE, C]]): Either[EE, List[C]] = {
		val matchedOption = find(a)({
			_ match {
				case l @Left(e) => true
				case _ => false
			}
		})

		matchedOption.map {
			_ match {
				case l @ Left(e) => Left(l.value)
				case r @ Right(v) => Right(::(r.value, Nil))
			}
		}.getOrElse {
			Right(
				foldLeft(a, Nil: List[C])({
					(b, a) => {
						a match {
							case l @ Left(e) => b
							case r @ Right(v) => ::(r.value, b)
						}
					}
				})
			)
		}
	}
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

// Real Application for map2
case class Person(val name: Name, val age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

class RealApp {
	def mkName(name: String): Either[String, Name] = {
		if (name.trim.isEmpty) Left("invalid name")
		else Right(new Name(name))
	}

	def mkAge(age: Int): Either[String, Age] = {
		if (age < 0) Left("invalid age")
		else Right(new Age(age))
	}

	// brilliant and idiomatic realization of Field Check
	def mkPerson(name: String, age: Int) = {
		mkName(name).map2(mkAge(age))(Person(_, _))
	}
}


