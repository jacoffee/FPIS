package ch06

/**
 * Created by allen on 1/11/15.
 */

/*


EXERCISE 13 (hard): To gain experience with the use of State, implement a simulation of a simple candy dispenser.
The machine has two types of input:
   You can insert a coin, or you can turn the knob to dispense candy.
   It can be in one of two states: locked or unlocked.
It also tracks how many candies are left and how many coins it contains.

*/

sealed trait Input
case object Coin extends Input // state for inserting a coin
case object Turn extends Input // state for getting a candy

case class Machine(locked: Boolean, candies: Int, coins: Int) { self =>
    /*
      Inserting a coin into a locked machine will cause it to unlock if there is any candy left.
      Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
      Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
      A machine that is out of candy ignores all inputs.

       Input => Input
       actually Input is to operations what action will be acted on the current machine and get the processed result
    */
    // TODO test it
   def simulateMachine(inputs: List[Input]): State[Machine, Int] = {
        def go(acc: Machine, inputs: List[Input]): State[Machine, Int] = {
            inputs match {
                case Nil => State(acc => (acc.coins, acc))
                case head :: tail => {
                    go({
                        if (acc.candies < 0) acc
                        else {
                            head match {
                                case Coin if (acc.locked) => Machine(false, acc.candies, acc.coins + 1)
                                case Turn if (!acc.locked) => Machine(true, acc.candies -1, acc.coins)
                                case _ => acc
                            }
                        }
                    }, tail)
                }
            }
        }
        go(self, inputs)
    }
}
