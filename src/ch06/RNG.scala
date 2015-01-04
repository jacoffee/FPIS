package ch06

/**
 * Created by allen on 15-1-4.
 */

/*
	typical method with side effect
	 java.util.Random
	a internal state is being maintained to generate next value

	instead of updating state with side effect
	we can return the new value && state as return value

*/
trait RNG {
	def nextInt: (Int, RNG)
}

object RNG {
	def simple(seed: Long): RNG = {
		new RNG {
			def nextInt = {
				//  linear congruential generator
				val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
				((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
			}
		}
	}

	// -2 31 方 -2 31-1
	Int.MinValue.abs
	/*
		EXERCISE 1: Write a function to generate a random positive integer. Note:
		you can use x.abs to take the absolute value of an Int x, Make sure to handle
		the corner case Int.MinValue, which doesn't have a positive counterpart
	*/
	def positiveInt(mg: RNG): (Int, RNG) = {
		simple(mg.nextInt._1).nextInt match {
			case (Int.MinValue, rng) => (0, rng)
			case (x, rng) => (x.abs, rng)
		}
	}

	// Dealing with awkwardness in FP

	/*
		EXERCISE 2: Write a function to generate a Double between 0 and 1,
		including 1 . Note: you can use Int.MaxValue to obtain the maximum positive
		integer value and
		you can use x.toDouble to convert an Int  x to a Double
		0 < x <= 1
	*/
	def double(rng: RNG): (Double, RNG) = {

		???
	}
}
