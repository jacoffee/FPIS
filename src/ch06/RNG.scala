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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    // -(n+1) is for being compatible with Range of Int in Scala -2 ^ 31 ~ 2 ^ 31 -1
    // If the value -2 ^ 31, directly use prefix -, will not make it positive, so the trick +1
    (if (n < 0) -(n+1) else n, r)
  }


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


  // RNG => (A, RNG)  -->  avoid pass RNG as parameter
  type Rand[+A] = RNG => (A, RNG)

  // Type lift, Rand is a wrapper for type A
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](random: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = random(rng)
      (f(a), rng2)
    }
  }

  /* EXERCISE 5: Use to map generate an Int between 0 and  n , inclusive: */
  // println(" positiveInt " + RNG.positiveInt(4)(init._2)._1)
  // the invocation is awakward
  def positiveMax(n: Int): Rand[Int] = {
    map(unit(n)) {
      a => scala.util.Random.nextInt(a) + 1
    }
  }

  /* EXERCISE 6: Use to map reimplement RNG.double in a more elegant way. */
  val _double: Rand[Double] = map(nonNegativeInt) {
    _ .toFloat / Int.MaxValue
  }

  /*
    EXERCISE 7 Write its implementation and then use it to reimplement the intDouble
    and doubleInt functions.

    combinator
  */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a1, r1) = ra(rng)
      val (b1, r2) = rb(r1)
      (f(a1, b1), r2)
    }
  }

  def intDoubleViaMap2 = map2(nonNegativeInt, double)((_, _))

  def triple(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    println(" triple n " + n)
    (n * 3, r)
  }

  def quadruple(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    println(" quadruple n " + n)
    (n * 4, r)
  }
  /*
    EXERCISE 8 (hard): If we can combine two RNG transitions, we should be
    able to combine a whole list of them. Implement , sequence for combining a
    List of transitions into a single transition.

    实现总结
    List[Rand[A]] --> 会将调用者传入的rng 依次丢进去, 然后调用
    val (a1, r1) = ra(rng)
    val (b1, r2) = rb(rng)
  */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      val l =  fs.map(_(rng))
      if (l.isEmpty) (Nil, rng)
      else (l.map(_._1), l.last._2)
    }
  }

  // 下面的流程通过foldRight的方式来积累原始值
  def _sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def _ints(count: Int): Rand[List[Int]] = {
    val rand: Rand[Int] = _.nextInt
    sequence(List.fill(count)(rand))
  }

  val rand: Rand[Int] = _.nextInt
  def intsViaSequence(count: Int)(rng: RNG) : (List[Int], RNG) = {
    val (n, r) = rng.nextInt
    sequence(List.fill(count)(rand))(r)
  }

  /* EXERCISE 9: Implement flatmap, then use it to reimplement positiveInt. */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (n, r) = f(rng)
      g(n)(r)
    }
  }

  def positiveIntViaFlatMap: Rand[Int] =
    flatMap(rand) { i =>
      if (i != Int.MinValue) unit(i.abs)
      else {
        // We want to retry the generator in the case of Int.MinValue, but we don't actually have an RNG
        rand
      }
    }


  /* EXERCISE 10: Reimplement .map and map2  in terms of flatMap */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => unit(f(a)) }
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }

    /*
      rng => {
        val (n, r) =  ra(rng)
        // g(n)(r)
        (
          flatMap(rb) { b=>
            unit(f(n, b))
          }
        )(r)
      }
    */
  }
}
