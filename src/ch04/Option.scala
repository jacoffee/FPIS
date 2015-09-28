package ch04

import java.util.regex.{ PatternSyntaxException, Pattern }
import ch03.{ List, Cons, Nil }
import ch03.List.{ exists, foldLeft }

/**
 * Created by allen on 14-12-5.
 */

// Here we are going to place our functions, when possible, inside the body of the Option trait, so they can be called with OO syntax
// obj.fn(arg1)
sealed trait Option[+A] {
  def get: A

  def isEmpty: Boolean

  def nonEmpty = !isEmpty
  def toList[A] = if (isEmpty) Nil else Cons(this.get, Nil)

  def map[B](f: A => B): Option[B] = {
    if (isEmpty) None else Some(f(this.get))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    if (isEmpty) None else f(this.get)
  }

  // compatibility
  // indicates the argument will not be evaluated until it is needed by the function
  // => B || B
  def getOrElse[B >: A](default: => B): B = {
    if (isEmpty) default else this.get
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    if (isEmpty) ob else this
  }
  def filter(f: A => Boolean): Option[A] = {
    if (isEmpty || !f(this.get)) None
    else  Some(this.get)
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def pattern(s: String): Option[Pattern] = {
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }
  }

  def mkMatcher(pat: String): Option[String => Boolean] = {
    pattern(pat) map { p =>
      (str: String) => p.matcher(pat).matches
    }
  }

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] = {
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)  // Function Lifting
  }

  /*
  A for-comprehension like this is simply syntax sugar. Internally, Scala will translate the above to ordinary method calls to map and  flatMap
  */
  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] = {
    mkMatcher(pat).flatMap { p1 =>
      mkMatcher(pat2).map { p2 =>
        p1(s) && p2(s)
      }
    }
  }

  // EXERCISE 2: Implement the function variance(if the mean is m, variance is the mean of math.pow(x - m, 2) , see ) in terms of anddefinition mean and flatMap.
  def variance(xs: Seq[Double]): Option[Double] = {
    // mean
    //Math.pow(x -m , 2)
    if (xs.isEmpty) None
    else {
      val mean = xs.sum / xs.size
      Some(
        (xs.map(x => Math.pow(x - mean, 2)).sum) / xs.size
      )
    }
  }

  // EXERCISE 3: bothMatch is an instance of a more general pattern. Write a
  // generic function map2,  that combines two Option values using a binary function.map2
  // If either Option value is None , then the return value is too.
  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] = {
    oa flatMap { a => ob map { b => f(a, b) } }
  }

  // EXERCISE 4: Re-implement bothMatch above in terms of this new function to the extent possible
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat1), mkMatcher(pat2)) {
      (p1, p2) => p1(s) && p2(s)
    }
  }

}
case class Some[A](value: A) extends Option[A] {
  def get = value
  def isEmpty = false
}
case object None extends Option[Nothing] {
  def get = throw new NoSuchElementException("None.get")
  def isEmpty = true
}

object Option {
  // find implicit conversion in Companion Object
  implicit def option2Iterable[A](xo: Option[A]): List[A] = xo.toList


  // def apply
  def unit[A](x: => A) = if (x == null) None else Some(x)
  def empty[A]: Option[A] = None

  /*
    EXERCISE 5: Write a functionsequence , that combines a list of Options
    into one option containing a list of all the values in the original list. If the
    original list contains None even once, the result of the function should be None
    otherwise the result should be with a list of all the values. Here is it signature
  */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if (exists(a)(elem => elem.isEmpty)) None
    else {
      // def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
      Some(
        foldLeft(a, Nil: List[A])((b, a) => Cons(a.get, b))
      )
    }
  }

  /*
    EXERCISE 6: Implement this function. It is straightforward to do using map
    and sequence, but try for a more efficient implementation that only looks at the
    list once.
     In fact, implement sequence in terms of  traverse
  */
  def sequence1[A](a: List[Option[A]]): Option[List[A]] = {
    None
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    None
  }

  // TODO  in the future
  /*
  EXERCISE 9: In this implementation, map2 is only able to report one error,
  even if both the name and the age are invalid. What would you need to change in
  order to report both errors? Would you change map2 or the signature of mkPerson?
  Or could you create a new data type that captures this requirement
  better than does, with some additional structure? How would ,Either orElse
  traverse sequence, and behave differently for that data type?
  */

}
