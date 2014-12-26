package ch04

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
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]



