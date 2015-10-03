package ch11

import ch03.{ List, ::, Nil }
import ch04.{ Option, Some, None }
import ch06.State

/*
    lift a function to take two parameters
    => map2 In Option
    def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
      oa flatMap { a => ob map { b => f(a, b) } }

    => map2 In Parser ...
    def map2[A,B,C](pa: Parser[A], pb: Parser[B])(f: (A,B) => C): Parser[C] =
      pa flatMap (a => pb map (b => f(a,b)))

    Actually Option[A], Parser[A] push us to generalize the map2 further

    ||
    ||
    ||

    use minial set of combinators in the common types to build the unified operation
*/

// all monads are functors, but not the other way around
trait Monad[F[_]] extends Functor[F] {

  /// one the minimal set of implementation for Monadic combinator
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma) { a => map(mb) { b => f(a, b) } }

  def factor[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  def cofactor[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(ma) => map(ma)(Left(_))
      case Right(mb) => map(mb)(Right(_))
    }

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
      val empty: F[List[A]] = unit(Nil)

      List.foldLeft(lma, unit(List[A]()))(
        // Attention the order of parameters in map2(A, B) || map2(A, B) will decide whether
       // the placeholder syntax can work out here
        // (accM, elemM) => map2(accM, elemM)(_ :: _)  not Okay f(a, b) => a :: b, so b must List
        (accM, elemM) => map2(elemM, accM)(_ :: _)
      )
  }

  // similiar definition in Scalaz Applicative => further abstraction thus more applicable

  /* however since we directly use map here(cause we assume the type is List), in Scalaz applicative
   this part has to be tweak to fit varibale types another data struture or abstraction seems inevitable
   Actually, it is the case in the Traverse.scala of Scalaz

      trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
        def traverse[A, G[_], B](value: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] =
      }

      Tranverse is Functor, also is good candidate for Type class, defining a set of methods whose implementation may vary on the type passing in
      if F is List, then listInstance is available for your use
      if F is Option, then  optionInstance is available for your use
  */

  def tranverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(List.map(la)(f))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(
      List.apply(scala.collection.immutable.List.fill(n)(ma): _*)
    )
  }


  // Heinrich Kleisli  function like this A => F[B] is called Kleisli Arrows
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    // A => F[A] A => F[A]
    (a: A) => flatMap(f(a))(g)
  }

  // then we can express associative law in a more symmetric way
  // compose(f, g), h == compose(f, compose(g, h))
  def _flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    // type A = Unit => F[A]
    // type B = A => F[B]

    compose(
      (_: Unit) => ma, f
    )(
      ()
    )
  }

  def identityLaw[A, B](ma: F[A])(f: A => F[B]): Boolean = {
    val reflex = flatMap(flatMap(ma)(f))((b: B) => unit(b)) // (=> A) => F[A]
    val reflex1 = flatMap(flatMap(ma)((a: A) => unit(a)))(f)

    val f1 = (a: A) => flatMap( flatMap(unit(a))(f) )((b: B) => unit(b))
    val f2 = (a: A) => flatMap(flatMap(unit(a))((a: A) => unit(a)))(f)
    f1 == f2
  }

  // def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
  def flatten[A](mma: F[F[A]]): F[A] = {
    // flatMap[F[A], B](mma)(ma => )
    flatMap(mma)(ma => ma)
  }

  def flatMapViaJoin[A, B](ma: F[A])(f: A => F[B]): F[B] = flatten(map(ma)(f))

  def composeViaFlatten[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
  // (a: A) => F[F[C]]
    (a: A) => {
      val mmb = map(unit(a))(f)
      flatten(map(mmb)(mb => flatten(map(mb)(g))))
    }

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val empty: F[List[A]] = unit(Nil)

    List.foldLeft(ms, empty)(
      (accM, elem) => map2(accM, f(elem))((acc, bool) =>
        if (bool) elem :: acc else acc
      )
    )
  }
}
/*
   monadic data types are those who implement the basic method method in monad
*/
case class Id[A](value: A) {
  // Heinrich Kleisli
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object MonadTest extends App {
  // Exercise 1
  // Associative law holds for Option
  val optMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option.unit(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f

    // that's why sequence is simple version of tranverse, cause it does not have the func part compared with tranverse
    override def sequence[A](lma: List[Option[A]]): Option[List[A]] = {
      Option.unit(
        List.flatMap(lma) { opt: Option[A] =>
          opt match {
            case Some(x) => ::(x, Nil)
            case None => Nil
          }
        }
      )
    }
  }

  // x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
  val optA = Some(3)
  val f1 = (x: Int) => Some(x + 1)
  val g1 = (x: Int) => Some(x - 3)

  assert( optA.flatMap(f1).flatMap(g1) == optA.flatMap(a => f1(a).flatMap(g1)) )


  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List.unit(a)
    def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = List.flatMap(ma)(f)
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = f(ma.value)
  }

  val result = Id("Hello, ").flatMap { a =>
    Id(" Monad").flatMap { b =>
       Id(a + b)
    }
  }

  val a = "Hello, "
  val b = " Monad"
  val result1 = a + b // bind value

  type IntState[A] = State[Int, A] // run: Int => (A, Int)

  // type lambda is usually(as my current level tell ) => A type constructor declared inline like this is often called a type lambda in Scala.
  // object IntStateMonad extends Monad[({ type IntState[A] = State[Int, A] })#IntState]
  object IntStateMonad extends Monad[({ type IntState[A] = State[Int, A] })#IntState] {
    def unit[A](a: => A): IntState[A] = State(i => (a, i))
    def flatMap[A, B](ma: State[Int, A])(f: A => State[Int, B]): IntState[B] = ma flatMap f
  }

  /*
     As we can see from this issue, cause State requires two type parameter so initially it is not a perfert candidate for Monad
     so we have to write StateMonad for each concrete type S in State[S, A] which would be very tedious, thanks to type lambda()
     we can abstract the case to a new level
  */
  class StateMonad[S] extends Monad[({ type lambda[A] = State[S, A] })#lambda] {

    def unit[A](a: => A): State[S, A] = State(i => (a, i))

    def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma flatMap f

    def getState: State[S, S] = State(s => (s, s))

    def setState(x: S): State[S, Unit] = State(_ => ((), x))
  }

   def stateMonad[S] = new Monad[({ type lambda[A] = State[S, A] })#lambda] {
    def unit[A](a: => A): State[S, A] = State(i => (a, i))
    def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma flatMap f
  }

   // Then S can be replaced with any valid conrete type: Int, String, ......

  import scala.collection.immutable.{ List, ::, Nil }
  val M = new StateMonad[Int]
  def zipWithIndex[A](as: List[A]): List[(Int,A)] = {
    // State[S, List[(Int, A)]] A --> List[(Int, A)]
    val ccc = as.map(a => State.unit[Int, A](a))

    as.foldLeft(M.unit(List[(Int, A)]()))(
      (acc, a) => {
        //  the implementation of flatMap is making sure that the current state is available to getState
        // A => State[S, B]
        acc.flatMap { xs =>

          M.getState.flatMap { n =>

              // State[S, Unit] --> State[Int, List[(Int, A)]] setState change the S in State[S, B] that acts as the returned value
              //  in flatMap f: A => State[S, B]
             M.setState(n + 1).map { any =>
               (n, a) :: xs
             }

          }

        }

        for {
          xs <- acc
          n <- M.getState
          _ <- M.setState(n + 1)
        } yield (n, a) :: xs

      }

    ).run(0)._1.reverse
  }

  val strList = "a" :: "b" :: "c" :: Nil
  val zip = zipWithIndex(strList)
  println(" zip " + zip)
}

case class Reader[R, A](run: R => A) {
  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = {
    Reader(
      // the initial r gets passed along the outer reader, resultant read from f
    // notice the diffrence between this and State is that the State Monad's S can be changed along the way but not this one
      (r: R) => f(run(r)).run(r)
    )
  }

}

object Reader {
  def monad[R] = new Monad[ ({type lambda[X] = Reader[R, X]})#lambda ] {

    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ma flatMap f
  }
}
