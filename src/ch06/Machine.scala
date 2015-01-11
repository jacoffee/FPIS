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

case class Machine(locked: Boolean, candies: Int) {

}
