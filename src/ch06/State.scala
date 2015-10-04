package ch06

/**
 * Created by allen on 15-1-6.
 */

/*
   The representation doesn't matter too much. What is important is that we have a single,
   general purpose type and using this type we can write general-purpose
   functions for capturing common patterns of handling and propagating state

   For this part, you have to extend your mind to that
   A ---> M[A]
   S => (A, S) ---> M[S => (A, S)] a function can also be wrapped
*/
object State {

  def point[S, A](a: A): State[S, A] =
    State((s: S) => (a, s))

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for { a <- sa; b <- sb } yield { f(a, b) }

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    val identity: State[S, List[A]] = point(Nil)
    sas.foldLeft(point[S, List[A]](List()))({
      (acc, elem) => elem.map2(acc)(_ :: _)
    })

    // CandyDispenser coins (3,9), candies Machine(true,9,3)
    sas.foldRight(point[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
  }

  // I just want bind the resultant State S into the returning A
  def get[S]: State[S, S] = State(s => (s, s))

  // with modification, the next state is the one we provide and value is ()
  // ignore the input S, set the returning S as the parameter S
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // self.map(_ => ())
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  // def apply[S, A](run: S => (A, S)): State[S, A] = new State(run)
}

case class State[S, +A](run: S => (A, S)) { self =>
  // type State[S, +A] = S => (A, S)

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

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for { a <- self; b <- sb } yield { f(a, b) }

  // Problems to solve: http://stackoverflow.com/questions/32410919/how-to-compose-two-different-state-monad
  type AppendBang[A] = State[Int, A]

  // Int => State[String, Int]
  type AddOne[A] = State[String, A]
  def addOne(n: Int): AddOne[Int] = State(s => (n + 1, s + "."))

  // String => State[Int, String]
  def appendBang(str: String): AppendBang[String] = State(s => (str + " !!!", s + 1))

  def myAction(n: Int)(initialAddOneState: String, initialAppendBangState: Int): ((String, Int), String) = {
    // A => State[S, A]
    val (addOneNum, newAddOneStr) = addOne(n).run(initialAddOneState)

    // B => State[S2, B]
    val (appendBangStr, newAppendBingState) = appendBang(newAddOneStr).run(initialAppendBangState)
    ((newAddOneStr, newAppendBingState), appendBangStr)
  }


  // Given  State[S, A], State[S2, B] ---> State[(S, S2), B] // first parameter S + S2 => (S, S2), second parameter A => B
  def compose[B, S2](func: B => State[S2, B])(convert: A => B): State[(S, S2), B] =
    State(
      ((s: S, s2: S2) => {
          val (a, s3) = run(s)
          val (b, s4) =  func(convert(a)).run(s2)
          (b, (s3, s4))
      }).tupled
    )
}