package ch06

/**
 * Created by allen on 10/4/15.
 */

/**
  Functional caching with explicit state passing.

   look the sigature here (xxx, c: Cache): (Cache, FollowerStats)

    S => (S, A) ---> State[S, A]

    xxx => State[S, A]

    So the code following could be refactored

*/


object ServiceTest2 {

  trait SocialService {
    def followerStats(u: String, c: Cache): (Cache, FollowerStats)
  }

  case class FollowerStats(username: String, numFollowers: Int, numFollowing: Int)
  case class Timestamped[A](value: A, timestamp: Long)

  case class Cache(stats: Map[String, Timestamped[FollowerStats]], hits: Int, misses: Int) {

    def get(username: String): Option[Timestamped[FollowerStats]] = stats.get(username)

    def update(u: String, s: Timestamped[FollowerStats]): Cache = Cache(stats + (u -> s), hits, misses)
  }

  object FakeSocialService extends SocialService {
    def followerStats(u: String, c: Cache) = {
      val (c1, ofs) = checkCache(u, c)
      ofs match {
        case Some(fs) => (c1, fs)
        case None => retrieve(u, c)
      }

    }

    private def checkCache(u: String, c: Cache): (Cache, Option[FollowerStats]) = {
      c.get(u) match {
        case Some(Timestamped(fs, ts))
          if !stale(ts) =>
          (c.copy(hits = c.hits + 1), Some(fs))
        case other =>
          (c.copy(misses = c.misses + 1), None)
      }
    }

    private def stale(ts: Long): Boolean = {
      System.currentTimeMillis - ts > (5 * 60 * 1000L)
    }

    private def retrieve(u: String, c: Cache): (Cache, FollowerStats) = {
      val fs = callWebService(u)
      val tfs = Timestamped(fs, System.currentTimeMillis)
      (c.update(u, tfs), fs)
    }

    private def callWebService(u: String): FollowerStats =
      FollowerStats(u, 0, 0)
  }

}
