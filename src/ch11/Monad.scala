package ch11

import ch03.{ List, Cons, Nil }
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

// ALL MONADS ARE FUNCTORS, but not the other way around
trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma) { a => map(mb) { b => f(a, b) } }

  def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    map2(ma, mb)((_, _))

  // Explain to yourself what it does.  Real Applicatioon
  def cofactor[A, B](e: Either[M[A], M[B]]): M[Either[A, B]] =
    e match {
      case Left(ma) => map(ma)(Left(_))
      case Right(mb) => map(mb)(Right(_))
    }

  def sequence[A](lma: List[M[A]]): M[List[A]] = {
      val empty: M[List[A]] = unit(Nil)

      List.foldLeft(lma, empty)(
        (accM, elemM) => map2(accM, elemM)((acc, elem) => elem :: acc)
      )
  }

  def tranverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = sequence(List.map(la)(f))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    sequence(
      List.apply(scala.collection.immutable.List.fill(n)(ma): _*)
    )
  }


  // Heinrich Kleisli  function like this A => M[B] is called Kleisli Arrows
  def lift[A, B](f: A => B): M[A] => M[B] = map(_)(f)

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = {
    // A => M[A] A => M[A]
    (a: A) => flatMap(f(a))(g)
  }

  // then we can express associative law in a more symmetric way
  // compose(f, g), h == compose(f, compose(g, h))
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    // type A = Unit => M[A]
    // type B = A => M[B]

    compose(
      (_: Unit) => ma, f
    )(
      ()
    )
  }

  def identityLaw[A, B](ma: M[A])(f: A => M[B]): Boolean = {
    val reflex = flatMap(flatMap(ma)(f))((b: B) => unit(b)) // (=> A) => M[A]
    val reflex1 = flatMap(flatMap(ma)((a: A) => unit(a)))(f)

    val f1 = (a: A) => flatMap( flatMap(unit(a))(f) )((b: B) => unit(b))
    val f2 = (a: A) => flatMap(flatMap(unit(a))((a: A) => unit(a)))(f)
    f1 == f2
  }

  // def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def flatten[A](mma: M[M[A]]): M[A] = {
    // flatMap[M[A], B](mma)(ma => )
    flatMap(mma)(ma => ma)
  }

  def flatMapViaJoin[A, B](ma: M[A])(f: A => M[B]): M[B] = flatten(map(ma)(f))

  def composeViaFlatten[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
  // (a: A) => M[M[C]]
    (a: A) => {
      val mmb = map(unit(a))(f)
      flatten(map(mmb)(mb => flatten(map(mb)(g))))
    }

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    val empty: M[List[A]] = unit(Nil)

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

object Monad {
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
            case Some(x) => Cons(x, Nil)
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
   def abstractStateMonad[S] = new Monad[({ type lambda[A] = State[S, A] })#lambda] {
    def unit[A](a: => A): State[S, A] = State(i => (a, i))
    def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma flatMap f
  }

   // Then S can be replaced with any valid conrete type: Int, String, ......
}
