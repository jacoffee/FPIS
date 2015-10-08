package ch06

import scalaz._
import scalaz.std._
import Scalaz._
import ch06.ServiceTest3.{FollowerStats, Cache}

/**
 * Created by allen on 10/5/15.
 */
object TypeClassIntegerationExample extends App {


  type StateCache[A] = State[Cache, A]

  import scalaz.syntax.id._
  // import scalaz.syntax.applicative._
  // Applicative[StateCache]
  /*
      implicit def ApplicativeIdV[A](v: => A) = new ApplicativeIdV[A] {
         lazy val self = v
      }

      point

      @inline def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F
  */
  implicit val monoidCache = new Monoid[Cache] {
    override def zero: Cache = Cache(Map.empty, 0, 0)
    override def append(f1: Cache, f2: => Cache): Cache =
     Cache(f1.stats ++ f2.stats, f1.hits + f2.hits, f1.misses + f2.misses)
  }

  implicit val applicativeCache = new Applicative[StateCache] {
    def point[A](a: => A): StateCache[A] = State.point(a)
    def ap[A, B](fa: => StateCache[A])(f: => StateCache[A => B]): StateCache[B] = fa.ap(f)
  }

  // Functor[StateCache].apply

  val r1: StateCache[Int] = 10.point[StateCache] //
  val r2 = State.point[Cache, Int](10)


  import ServiceTest3._
  import FakeSocialService._
  val listStateCache: List[StateCache[FollowerStats]] =
    List(
      followerStats("u1"),
      followerStats("u2"),
      followerStats("u1")
    )


  val stateOfList = State.sequence[Cache, FollowerStats](listStateCache)
  val (followers, stateCache) = stateOfList.run(Cache.empty)
  println(" hits & misses " + stateCache.hits + " ::: " + stateCache.misses)
}

