package sz

import scalaz._
import Scalaz._
import scalaz.syntax.IdOps
// functions and type class instances for Option and List
import scalaz.std._
import scalaz.std.option._

object MonadBasics extends App {
  //////// Verify basic laws of Monad in Scalaz

  // say no to the "Is a collection with flatMap a monad?" by validating the Monadic Law
  val f: String => Iterable[String] = s => List(s)
  val g: String => Iterable[String] = s => Set(s)
  val h: String => Iterable[String] = s => List("hi", "hi")

  // M[F].flatMap(f).flatMap(g)
  // x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

  val list = List("a", "a")
  val r1 = list.flatMap(f).flatMap(g)
  val r2 = list.flatMap(a => f(a).flatMap(g))

  println(" r1 " + r1)
  println(" r2 " + r2)

  val r11 = for {
     a <- f("hi")
     b <- g(a)
     c <- h(b)
  } yield c

  val sameWithR11 = {
    f("h1").flatMap { a =>
      // implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Set[A]] = setCanBuildFrom[A]
       g(a).flatMap { b =>
          h(b) // List("h1", "h1")
       }
    }
  }

  val r22 = for {
    a <- f("hi")
    c <- h(a)
    b <- g(c)
  } yield b

  println(" r11 " + r11)
  println(" r22 " + r22)

  // according to the associativity law in Monadic law, the order of f g h should not matter
  // or exactly flatMap should follow the associative law

  // >>= equals flatMap
  import scalaz.std.option.optionInstance

  Monad[Option].point(3) >>= (x => (x + 1000).some)

  //implicit def ToIdOps[A](a: A): IdOps[A] = new IdOps(a)
  3 |> (x => (x + 1000).some)

//  (Some(3) flatMap Monad[Option].point[Int]) == Some(3)
//  Monad[Option].bind(Some(3))(Monad[Option].point[Int]) == Some(3)

  // ("move on up".some flatMap {Monad[Option].point(_)}) assert_=== "move on up".some

  // Monad[List].point(3) >>=
}
