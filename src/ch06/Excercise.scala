package ch06

/**
 * Created by allen on 15-1-4.
 */
object Excercise extends App {
	println(" pure random " + RNG.simple(3).nextInt._1)
	println(" pure random " + RNG.simple(3).nextInt._1)
	// the upper is the same --> transparency referential


	println(" positiveInt " + RNG.positiveInt(RNG.simple(-10).nextInt._2)._1)
}
