package ch06

import scala.annotation.tailrec

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
	// 生成RNG实例的时候 会调用一个nextInt
	def simple(seed: Long): RNG = {
		new RNG {
			def nextInt = {
				//  linear congruential generator
				val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
				((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
			}
		}
	}

	case class Simple(seed: Long) extends RNG {
		def nextInt: (Int, RNG) = {
			val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
			val nextRNG = Simple(newSeed)
			val n = (newSeed >>> 16).toInt
			(n, nextRNG)
		}
	}


	Int.MinValue.abs
	/*
		EXERCISE 1: Write a function to generate a random positive integer. Note:
		you can use x.abs to take the absolute value of an Int x, Make sure to handle
		the corner case Int.MinValue, which doesn't have a positive counterpart
	*/
	def positiveInt(rng: RNG): (Int, RNG) = {
		simple(rng.nextInt._1).nextInt match {
			case (Int.MinValue, rng) => (0, rng)
			case (x, rng) => (x.abs, rng)
		}
	}

	// -2 31 方 -2 31-1
	def nonNegativeInt(rng: RNG): (Int, RNG) = {
		val (n, r) = rng.nextInt
		(if (n < 0) -(n+1) else n, r)  // -(n+1) is compatiable for Int.MinValue
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
		val (n, r) = rng.nextInt
		val (n1, r1) = nonNegativeInt(r)
		(n1.toDouble / Int.MaxValue, r1)
	}

	/*
		EXERCISE 3: Write functions to generate an (Int, Double)  pair, a
		(Double, Int)  pair, and a (Double, Double, Double)  3-tuple. You
		should be able to reuse the functions you've already written
	*/
	def intDouble(rng: RNG): ((Int,Double), RNG) = {
		val (n, r) = rng.nextInt
		val (d, r1) = double(rng)
		((n, d), r1)
	}

	def doubleInt(rng: RNG): ((Double,Int), RNG) = {
		val ((n, d), r) = intDouble(rng)
		((d, n), r)
	}

	def double3(rng: RNG): ((Double,Double,Double), RNG) = {
		val (d1, r) = double(rng)
		val ((_, d2), r1) = intDouble(r)
		val ((d3, _), r2) = doubleInt(r1)
		((d1, d2, d3), r2)
	}

	/* EXERCISE 4: Write a function to generate a list of random integers. */
	def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
		@tailrec def go(count: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
			if (count == 0) (acc, rng)
			else {
				val (n, r2) = rng.nextInt
				go(count - 1, n :: acc, r2)
			}
		}
		go(count, Nil, rng)
	}
}
