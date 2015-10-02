package ch06

/**
 * Created by allen on 15-1-4.
 */
object Excercise extends App {
  val init = RNG.Simple(100).nextInt

  println(" pure random " + init._1)
  println(" pure random " + init._1)
  // the upper is the same --> transparency referential


  println(" positiveInt " + RNG.positiveInt(init._2)._1)

  println(" positiveIntViaFlatMap " + RNG.positiveIntViaFlatMap(init._2)._1)
  println(" nonNegativeInt " + RNG.nonNegativeInt(init._2)._1)

  println(" double " + RNG.double(init._2)._1)

  println(" intDouble " + RNG.intDouble(init._2)._1)
  println(" intDoubleViaMap2 " + RNG.intDoubleViaMap2(init._2)._1)

  println(" ints " + RNG.ints(4)(init._2)._1)

  println(" hide RNG state")
  println(" positiveMax " + RNG.positiveMax(4)(init._2)._1)

  //List(RNG.nonNegativeInt _, RNG.triple _)
  // 类型推断问题
  println(" sequence " + RNG.sequence[Int](RNG.nonNegativeInt _ :: RNG.triple _ :: Nil)(init._2)._1)
  println(" _sequence " + RNG._sequence[Int](RNG.nonNegativeInt _ :: RNG.triple _ :: Nil)(init._2)._1)

  println(" intsViaSequence " + RNG.intsViaSequence(3)(init._2)._1)

  println(" _ints " + RNG._ints(3)(init._2)._1)

  println(" positiveIntViaFlatMap " + RNG.positiveIntViaFlatMap(init._2)._1)

  println(" map2 " + RNG.map2(RNG.triple, RNG.quadruple)(_ + _)(init._2)._1)
  println(" map2ViaFlatMap " + RNG.map2ViaFlatMap(RNG.triple, RNG.quadruple)(_ + _)(init._2)._1)

  ////// State Test
  println(" after two coins ")
  val machine = Machine(true, 10, 2)

  val changedState = machine.simulateMachine(Coin :: Turn :: Nil)
  val changedStateViaFold = machine.simulateMachineViaFold(Coin :: Turn :: Nil)

  val (coins, machine1) = changedState.run(machine)
  val (coinsViaFold, machine2) = changedStateViaFold.run(machine)

  println(" coins " + coins + " , canides " + machine1.candies)
  println(" coinsViaFold " + coinsViaFold + ", candies " + machine2.candies)

  val (co, ca) = CandyDispenser.simulateMachine(Coin :: Turn :: Nil).run(machine)
  println(" CandyDispenser coins " + co +", candies " + ca)

}
