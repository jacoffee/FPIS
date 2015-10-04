package ch06


/**
    reshape the code with State[S, A]
*/

object ServiceTest3 {

  // type StateCache = State[Cache, FollowerStats]

  trait SocialService {
    def followerStats(u: String): State[Cache, FollowerStats]
  }

  case class FollowerStats(username: String, numFollowers: Int, numFollowing: Int)
  case class Timestamped[A](value: A, timestamp: Long)

  case class Cache(stats: Map[String, Timestamped[FollowerStats]], hits: Int, misses: Int) {

    def get(username: String): Option[Timestamped[FollowerStats]] = stats.get(username)

    def update(u: String, s: Timestamped[FollowerStats]): Cache = Cache(stats + (u -> s), hits, misses)
  }


  object FakeSocialService extends SocialService {

    // (ServiceTest3.Cache, ServiceTest3.FollowerStats)
    def followerStats(u: String): State[Cache, FollowerStats] = {

      checkCache(u) flatMap { fsOpt =>
        fsOpt.map(State.point[Cache, FollowerStats]).getOrElse(retrieve(u))

        /*
        fsOpt match {
          // case Some(fs) => State(s => (fs, s)) // weird part, ignoring the inital State
          case Some(fs) => State.point(fs)
          case None => retrieve(u)
        }
        */
      }

      // the following code actually run the state, which could be avoided
      State(
        (c: Cache) => {
          val (fsOpt, c1) = checkCache(u).run(c)
          fsOpt match {
            case Some(fs) => (fs, c1)
            case None => retrieve(u).run(c)
          }
        }
      )
    }

    private def checkCache(u: String): State[Cache, Option[FollowerStats]] = {

      // initial version
      State(
        (c: Cache) => { // obtain Cache
          c.get(u) match {
            case Some(Timestamped(fs, ts))
              if !stale(ts) =>
              (Some(fs), c.copy(hits = c.hits + 1)) // update Cache
            case other =>
              (None, c.copy(misses = c.misses + 1))
          }
        }
      )

      // updated version
      for {
        cache <- State.get[Cache]
        optFS <- State.point(
          cache.get(u).collect {
            // if not stale, Option[Timestamped]
            case Timestamped(fs, ts) if stale(ts) => fs
          }
        )
        // if optFs is Some, hit +1, else misses +1 update cache States
        _ <- State.set(
          optFS.fold(cache.copy(misses = cache.misses + 1))(
            _ => cache.copy(hits = cache.hits + 1)
          )
        )
      } yield(optFS)


    }

    private def stale(ts: Long): Boolean = {
      System.currentTimeMillis - ts > (5 * 60 * 1000L)
    }

    private def retrieve(u: String): State[Cache, FollowerStats]  = {
      State(
        (c: Cache) => {
          val fs = callWebService(u)
          val tfs = Timestamped(fs, System.currentTimeMillis)
          (fs, c.update(u, tfs))
        }
      )
    }

    private def callWebService(u: String): FollowerStats =
      FollowerStats(u, 0, 0)
  }
}
