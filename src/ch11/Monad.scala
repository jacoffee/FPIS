package ch11


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
}

object Monad {
  import ch03.List
  import ch04.Option

  // Exercise 1
  val optMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option.unit(a)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List.unit(a)
    def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = List.flatMap(ma)(f)
  }

  // Exercise 2
}
