package ch06

/**
 * Created by allen on 15-1-6.
 */

/*
	EXERCISE 11: Generalize the functions "unit","map","map2","flatMap",
	"sequence."  Add them as methods on the case class where possible.State
	Otherwise you should put them in a companion object.
 */

case class State[S, +A](run: S => (A, S)) {
  // type State[S, +A] = S => (A, S)

  def unit[A](a: A): State[S, A] =
    State((s: S) => (a, s))

  // covariant type A occurs in contravaraint position in type A of value a
  //def map[B](a: A) = ???
  // X < : A => C[X] < : C[A] for every method should work that worked on the original class
  // if C[A] or method m(a: A) it worked on every A
  // if C[X] is can work on every X, not every A
  // real Example
  /*
  class Cell[+T] (init: T) {
    private[this] var current = init
    def get = current
    def set(x: T) { current = x }
  }

    object Cell {
      val c1 = new Cell[String] ("abc")
      val c2: Cell[Any] = c1
      c2和c1 指向了同一块内存区域

      c2.set(1)
      val s: String = c1.get
    }
    But taken together, these four lines end up assigning the integer 1 to the string s. This is clearly a violation of type soundness

  */


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

//object State {
//	def get[S]: State[S, S] = State(s => (s, s))
//	def set[S](s: S): State[S, Unit] = State(_ => ((), s))
//}