package sz

import scalaz._
import Scalaz._

import scalaz.Id._


object StateMonadBasics extends App {
    val stateMonad: State[Int, String] = State(i => (i + 1, i.toString))
   // Id.Id[String] => String

    /** Run, discard the final state, and return the final value in the context of `F` */
    val (finalState, finalValue) = stateMonad(1)
    val result1 = stateMonad.eval(1)
    val result2 = stateMonad.exec(1)
    assert(finalState == result2)
    assert(finalValue == result1)

    // runZero(implicit S: Monoid[S]) Monoid[Int]
     println(" stateMonad.runZero " + stateMonad.runZero[Int])
     println(" stateMonad.run(0) " + stateMonad.run(0))

    import java.util.Random
    def dice(): State[Random, Int] = State(r => (r, r.nextInt(6) + 1))

    dice().flatMap { d1 =>
      val ccc = dice().map { d2 =>
        (d1, d2)
      }

      ccc
    }

   val twoDice = (for {
     d1 <- dice()
     d2 <- dice()
   } yield {
       (d1, d2)
   })

  val (randomState, value) = twoDice(new Random(2L))

  // sequence List[State[S, (Int, Int)]]

  //  List[IndexedStateT[Id.Id, Random, Random, (Int, Int)]] => IndexedStateT[Id.Id, Random, Random, List(Int, Int)]

  import scalaz.std.list.listInstance
  def applicative[A] = Applicative[({type RandomState[A] = State[Random, A]})#RandomState]
  val twoDices = List.fill(10)(twoDice)
  val tenDoubleThrows = applicative.sequence(twoDices)(listInstance)

  // what if the Option
  import scalaz.std.option.optionInstance

  // Option[scalaz.State[Random, Int]]
  val diceOpt = Option(dice())

  // type ascription
  applicative.sequence(diceOpt): State[Random, Option[Int]]

  val (tenDoubleThrowsResult, tenDoubleThrowProcess) = tenDoubleThrows(new Random(2L))

  println(" Ten Double Throws " + tenDoubleThrowProcess)


  /**
    def traverse[A, G[_], B](value: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] =
      G.traverse(value)(f)(this)

      where is the implicit G comes from
   */




}
