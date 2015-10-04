package ch06

/**
  examples in  presentation from https://www.youtube.com/watch?v=Jg3Uv_YWJqI
  kind of a vivid example to avoid mutable state with State monad in the real application

  The example is about getting followers of the user on social media site, cause there could be millions of followers of the certain users
  whose basic info will not frequently change, fetching them every time the client sends a request would incur considerable overhead.

  In traditional imprerative programming domain, usually we just cache the follower stats in some internal stucture(map or something like that) which
  would cause lots of problem on updating mutally shared state.

  So here in the realm of FP, we are trying to make stateful APIs purely functional
*/

object ServiceTest1 {

  trait SocialService {
    /**
     * Retrieves the following statistics
     * for the specified user.
     */
    def followerStats(username: String): FollowerStats
  }

  case class FollowerStats(username: String, numFollowers: Int, numFollowing: Int)

  case class Cache(stats: Map[String, FollowerStats], hits: Int, misses: Int) {

    def get(username: String): Option[FollowerStats] = stats.get(username)

    def update(u: String, s: FollowerStats): Cache = Cache(stats + (u -> s), hits, misses)
  }

  object FakeSocialService extends SocialService {
    def followerStats(username: String) = FollowerStats(username, 0, 0)
  }

}
