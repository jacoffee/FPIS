package ch06

/**
 * Created by allen on 1/11/15.
 */
sealed trait Input
case object Coin extends Input // state for inserting a coin
case object Turn extends Input // state for getting a candy

import State._
object CandyDispenser {

  def update: Input => Machine => Machine = (input: Input) => (machine: Machine) => {
    (input, machine) match {
      case (_, m @ Machine(_, candies, _)) if candies < 0 => m
      case (Coin, Machine(true, candies, coins))  => Machine(false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins))  => Machine(true, candies - 1, coins)
      case (_, _) => machine
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    // another idomatic way to solve, modify get & set, but that returns State[S, Unit],
    // so change the order would do the magic set and get

    // Input => State[Machine, Int]
    // compose[A](g: A => T1): A => R = { x => apply(g(x)) }

    for {
        _ <- sequence(
          // List[State[Machine, Unit]]
         inputs.map { input =>
           (modify[Machine] _ compose update)(input)
         }
        )
       m <- get // cause the flatMap automatically infer type so get can be invoked directly
    } yield {
      (m.coins, m.candies)
    }
  }
}

case class Machine(locked: Boolean, candies: Int, coins: Int) { self: Machine =>
    /*
      Inserting a coin into a locked machine will cause it to unlock if there is any candy left.
      Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
      Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
      A machine that is out of candy ignores all inputs.

       Input => Input
       actually Input is to operations what action will be acted on the current machine and get the processed result
    */
    // TODO test it
   def initialState: State[Machine, Int] = State(s => (s.coins, s))

   def simulateMachine(inputs: List[Input]): State[Machine, Int] = {
        def go(machine: Machine, inputs: List[Input]): State[Machine, Int] = {
            inputs match {
                case Nil =>
                  // pass the current S i.e Machine as the result
                  State(acc => (machine.coins, machine))
                case head :: tail => {
                    go({
                        if (machine.candies < 0) machine
                        else {
                            head match {
                                case Coin if (machine.locked) => Machine(false, machine.candies, machine.coins + 1)
                                case Turn if (!machine.locked) => Machine(true, machine.candies -1, machine.coins)
                                case _ => machine
                            }
                        }
                    }, tail)
                }
            }
        }
        go(self, inputs)
    }


  /*
    State change rather than simply the machine change
    val changedMachine = inputs.foldLeft(self)({
      (machine, input) => {
        if (machine.candies < 0) machine
        else {
          input match {
            case Coin if (machine.locked) =>
              Machine(false, machine.candies, machine.coins + 1)
            case Turn if (!machine.locked) =>
              Machine(true, machine.candies - 1, machine.coins)
            case _ => machine
          }
        }
      }
    })
    State(machine => (machine.coins, machine))
  */

  def simulateMachineViaFold(inputs: List[Input]): State[Machine, Int] = {
    val initialState: (Machine, State[Machine, Int]) = (self, State(self => (self.coins, self)))
    inputs.foldLeft(initialState)({
      (acc, input) => {
        val (machine, _) = acc
        if (machine.candies < 0) acc
        else {

          input match {
            case Coin if (machine.locked) => {
              val changedMachine = Machine(false, machine.candies, machine.coins + 1)
              (changedMachine, State(_ => (changedMachine.coins, changedMachine)))

            }
            case Turn if (!machine.locked) => {
              val changedMachine = Machine(true, machine.candies - 1, machine.coins)
              (changedMachine, State(_ => (changedMachine.coins, changedMachine)))
            }
            case _ => acc
          }
        }
      }
    })._2
  }
}
