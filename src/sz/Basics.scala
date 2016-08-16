package sz

import scalaz._
import Scalaz._
// func tions and type class instances for Option and List
import scalaz.std._
//import std.option._

object Basics extends App {

	 // type XX[Int] = Int
	 // 0: Id[Int]

	/*
		def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
            ap(fb)(map(fa)(f.curried))

        def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B]
	*/

	import scalaz.std.option.optionInstance
	/*
		def apply[F[_]](implicit F: Apply[F]): Apply[F] = F

		the implicit here we pass is  scalaz.std.option.optionInstance
    */
	Apply[Option].apply2(some(1), some(2))((a, b) => a < b)


	(none: Option[String]) |+| "andy".some

	////////// Equal => type safe equality check
    1 === 1


	// not equals
	1.some =/= 1.some

	// Due to special precedence rule /== is recognized as an assignment operator because it ends with = and does not start with =
	// 1 /== 3 && false
	1 =/= 3 && false


	import scalaz.std.string.stringInstance
	Equal.apply[String].equal("x", "y")

	//////////

	////////// Order => Scalaz equivalent for the Ord typeclass is Order
	import Order._
	"xxx" > "xyz"
	1.0 max 2.0

	// scala.math.Ordering => scalaz.Ordering
	import scalaz.Ordering
	(1.0 ?|? 2.0).toInt
	//////////


	////////// Show

	Show.show((_: String) => "test show")

	//////////



}
