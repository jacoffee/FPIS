package ch11

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = {
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
  }
}

object FunctorTest extends App {
  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }

  val optFunctor = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa map f
  }

  val list = List(("a",1), ("b", 2))
  assert(list.unzip == listFunctor.distribute(list))

  val some = Some(("a", 1))

}
