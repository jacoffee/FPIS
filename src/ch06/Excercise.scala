package ch06

/**
 * Created by allen on 15-1-4.
 */
object Excercise extends App {
	println(" pure random " + RNG.simple(3).nextInt._1)
	println(" pure random " + RNG.simple(3).nextInt._1)
	// the upper is the same --> transparency referential


	println(" positiveInt " + RNG.positiveInt(RNG.simple(0).nextInt._2)._1)

	println(" double " + RNG.double(RNG.simple(1).nextInt._2)._1)

	println(" intDouble " + RNG.intDouble(RNG.simple(1).nextInt._2)._1)

	println(" ints " + RNG.ints(4)(RNG.simple(3).nextInt._2)._1)
}
