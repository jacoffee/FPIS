package ch06

/**
 * Created by allen on 15-1-6.
 */

/*
   The representation doesn't matter too much. What is important is that we have a single,
   general purpose type and using this type we can write general-purpose
   functions for capturing common patterns of handling and propagating state
*/
case class State[S, +A](run: S => (A, S)) {
  // type State[S, +A] = S => (A, S)

  def unit[A](a: A): State[S, A] =
    State((s: S) => (a, s))

  def map[B](f: A => B): State[S, B] = {
    State(
      (s1: S) => {
        val (a1, s2) = run(s1)
        (f(a1), s2)
      }
    )
  }

  def flatMap[B](g: A => State[S, B]): State[S,B] = {
      State(
        (s: S) => {
          val (a1, s2) = run(s)
          g(a1).run(s2)
        }
      )
  }

  def map2[A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for { a <- sa; b <- sb } yield { f(a, b) }
}