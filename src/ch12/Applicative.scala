package ch12

import ch11.Functor
import java.util.Date

/**
 * Created by allen on 10/3/15.
 */

/**
map unit join
      map <- unit & apply || map2 & unit

      A monad is just an applicative functor with an additional combinator, join.
*/
object Applicative {

  def product[F[_], G[_]](fApp: Applicative[F], gApp: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    type lambda[X] = (F[X], G[X])
    new Applicative[lambda] {
      def unit[A](a: => A): lambda[A] = (fApp.unit(a), gApp.unit(a))
      def apply[A, B](fgab: lambda[A => B])(fab: lambda[A]): lambda[B] = {
        val (f2ab, g2ab) = fgab
        val (fa, fb) = fab
        (fApp.apply(f2ab)(fa), gApp.apply(g2ab)(fb))
      }
    }
  }

  // F[_] G[_] are applicative functors, then F[G[_]]
//  def compose[F[_], G[_]](fApp: Applicative[F], gApp: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
//    type lambda[X] = F[G[X]]
//
//    new Applicative[({type f[x] = F[G[x]]})#f] {
//      override def unit[A](a: => A): F[G[A]] = fApp.unit(gApp.unit(a))
//
//      // F[ G[A => B] ] F[ G[A] ]
//      override def apply[A, B](lab: F[ G[A => B] ])(la: F[ G[A] ]): F[G[B]] = {
//        map2[G[A => B], G[A], G[B]](lab, la)({
//          (gab, ga) => super.apply(gab)(ga)
//        })
//      }
//    }
//  }

}

trait Applicative[F[_]] extends Functor[F] { self =>

  def unit[A](a: => A): F[A]

   // kind of a new concept in Applicative Functor
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  // map2 in terms of map && apply
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    // B could B =>C
    // def map[A, B](fa: F[A])(f: A => B): F[B]
    (apply(map(fa)(f.curried)): F[B] => F[C])(fb)
  }

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    // f.curried A => B => C => D --> F[A => B => C => D]
    // (F[A => B => C => D])(F[A]) ---> F[B => C => D]

    apply( apply( apply(unit(f.curried))(fa) )(fb) ) (fc)
  }

  // map in terms of map2 & apply
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply[A, B](unit(f))(fa)

  // map2 apply unit
  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    val empty: F[List[A]] = unit(Nil)
    fas.foldLeft(unit(List[A]()))(
      (accM, elemM) => map2(elemM, accM)(_ :: _)
    )
  }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

  // applicative law ==> identity law
  def identityLaw[A](fa: F[A]) = {
    map(fa)(x => x) == fa
    unit((x:A) => x)

    // law of identity
    apply[A, A]( unit((x:A) => x) )(fa) == fa
    /**
      The second thing that the laws demand of applicative functors is that
          they preserve function composition. This is stated as the composition law:

      this is coming from the comparison between flatMap vs apply
      flatMap sometimes will stop the func chain from going further but apply will not
    */
  }

  // applicative law ==> composition law
  // f&g --> x === g(f(x))
  def compositionLaw[A, B, C](f: F[A => B], g: F[B => C], fa: F[A]) = {
    // A => B, B => C --> A => C, fa
    val fac: F[A => C] = map2(f, g)((ab, bc) => (ab andThen bc))
    apply(fac)(fa): F[C]
    // g x --> f (g x)
    apply(g)(apply(f)(fa)): F[C]

    // A => B B => C A
    map3(g, f, fa)((x1, x2, x3) => x1(x2(x3))): F[C]
  }
}


// Validation that is very much like Either except that it can explicitly handle more than one error
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: List[E]) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object ValidationTest extends App {

  def validationApplicative[E]: Applicative[({type f[A] = Validation[E,A]})#f] = {
    new Applicative[({type f[A] = Validation[E,A]})#f] {
      def unit[A](a: => A) = Success(a)
      def apply[A, B](vfab: Validation[E, (A) => B])(va: Validation[E, A]): Validation[E, B] = {
        (va, vfab) match {
          case (Success(a), Success(fab)) => Success(fab(a))
          case (Success(_), failure @ Failure(_, _)) => failure
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, h2 :: t1 ::: t2)
          case (failure @ Failure(h1, t1), _) => failure
        }
      }
    }
  }

  def validName(name: String): Validation[String, String] =
    if (name.trim.nonEmpty) Success(name)
    else Failure("Name cannot be empty", List())

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      val simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")
      simpleDateFormat.setLenient(false)
      Success(simpleDateFormat.parse(birthdate))
    } catch {
      case e: Exception => Failure("Birthdate must be in the form yyyy-MM-dd", List())
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits", List())

  def validate(name: String, birthdate: String, phone: String): Validation[String, WebForm] = {
    // Name => Birthdate => Phone => WebForm  A => B ---> B could be anything, if it's a func, the whole store gets interesting

    /*
      closure actually plays an important role, the context when it defines rather than the context when it is invoked

      scala> val sum3 : (Int, Int, Int) => Int = (_ + _ + _)
      sum3: (Int, Int, Int) => Int = <function3>

      scala> sum3.curried(1)
      res6: Int => (Int => Int) = <function1>

      scala> sum3.curried(1)(2)
      res7: Int => Int = <function1>

      scala> sum3.curried(1)(2)(3)
      res8: Int = 6
    */

    val va = validationApplicative[String]
    val webFormFunc = (a: String, b: Date, c: String) => WebForm(a, b, c)
    va.apply(
      va.apply(
        va.apply(va.unit(webFormFunc.curried))(validName(name))
      )(validBirthdate(birthdate))
    )(validPhone(phone))
  }

  println(" validate test")
  validate("", "2015-10-09333", "14567879878") match {
    case Success(w) => println("  WebForm Value " + w)
    case Failure(h, t) => println(" WebForm Failure " + (h :: t))
  }

  // Applicative Law
  def testApplicativeLaw[E] = {

  }
}

