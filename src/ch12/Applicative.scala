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
trait Applicative[F[_]] extends Functor[F] {

   // map0
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

}


// Validation that is very much like Either except that it can explicitly handle more than one error
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: List[E]) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object ValidationTest extends App {
  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty", List())

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case e: Exception => Failure("Birthdate must be in the form yyyy-MM-dd", List())
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits", List())
}

