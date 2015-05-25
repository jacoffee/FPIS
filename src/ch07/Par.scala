package ch07

import java.util.concurrent.{ ForkJoinPool, ExecutorService }

import scala.collection.mutable.ListBuffer

// compatiable with Future -- unitFuture
import java.util.concurrent._

/**
 * Created by allen on 1/17/15.
 */


/*
	sequential computing - blocking
	parallelized
*/

object Par {
	type Par[A] = ExecutorService => Future[A]

	implicit val ex: ExecutorService = new ForkJoinPool()

	def run[A](a: Par[A])(implicit ex: ExecutorService): Future[A] = a(ex)
	/*
		actually get: A extends get() in Future
	*/
	private case class UnitFuture[A](get: A) extends Future[A] {
		override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

		override def isCancelled: Boolean = false

		override def get(timeout: Long, unit: TimeUnit): A = get

		override def isDone: Boolean = true
	}


	def unit[A](a: A): Par[A] = (ex: ExecutorService) => UnitFuture(a)
	def async[A](a: => A): Par[A] = fork(unit(a))

	// 当你想到看到map2的时候，你就可以顺带的想象，我能否对一个并行计算进行进一步的操作，就像对Future进行mapTo操作
	def sortPar(l: Par[List[Int]]): Par[List[Int]] =
		map2(l, unit(()))((a, _) => a.sorted) // (A, B) 实际上是从平行计算里面取出来的结果，所以不需要进一步的5

	def mapViaMap2[A, B](l: Par[A])(f: A => B): Par[B] =
		map2(l, unit(()))((a, _) => f(a))

	def sortParViaMap(l: Par[List[Int]]): Par[List[Int]] =
		mapViaMap2(l)(_.sorted)

	// 按照书中的逻辑，既然map2的操作首先是分别进行两个并行计算 然后再汇总结果那么我们可以对它进行拆分
	def product[A, B](a: Par[A], b: Par[B]): Par[(A, B)] =
		(ex: ExecutorService) => UnitFuture((a(ex).get, b(ex).get))

	def map[A, B](fa: Par[A])(f: A => B): Par[B] =
		(ex: ExecutorService) => UnitFuture(f(fa(ex).get))

	def map2ViaComposite[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
		map(product(a, b))(x => f(x._1, x._2))

	// on the other hand, i didn't realize that fork actually means spawning off a new thread to do something
	def fork[A](a: => Par[A]): Par[A] = {
		es => es.submit(new Callable[A] {
			def call(): A = a(es).get
		})
	}

	/*
		我们一般对collection进行map处理的时候都会 sequential process 但是
		要想进行并且运行 我们需要在每一个运算的起一个并行运算
		在本例中就是 f: A => B convert to f: A => Par[B]
	*/
	def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => async(f(a))

	/*
		EXERCISE 7 (hard): Let's write this function, typically called sequence. No additional primitives are required.
		这让我想起future中的方法 List[Future[A]] => Future[List[A]]
	*/

	def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
		(ex: ExecutorService) => {
			val a1 = a(ex).get
			val b1 = b(ex).get
			UnitFuture(f(a1,b1)) // 类似于scala Future中的mapTo
		}

	def sequence[A](l: List[Par[A]]): Par[List[A]] = {
		// 这种想法是绝对错的，因为它调用了run，而sequence实际上是没有运算
		l.foldRight(unit(Nil: List[A]))({
			(p1, u) => map2(p1, u)({
				(v, uu) => v :: uu
			})
		})
	}

	/*
		After a call to run
		it will fork a single asynchronous computation which itself spawns N parallel computations
		then waits for these computations to finish, collecting their results up into a list
	*/
	def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
		val fbs: List[Par[B]] = l.map { a =>
			asyncF(f)(a)
		}
		sequence(fbs)
	}

	// 如何收集 通过predicate的判断东西 || 是否需要引入  抽象数据结构 null => Some
	//  List[A] => List[A => List[A]] => flatten
	def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = fork {

		val lpl: List[Par[List[A]]] = l.map { a =>
			asyncF( (a: A) => { if (f(a)) List(a) else List() })(a)
		}

		map(sequence(lpl))(_.flatten)
	}
}


trait Par[+A] {
	// injects a constant into a parallel computation
	def unit[A](a: A): Par[A]

	def get[A](a: Par[A]): A // 执行并行运算并收集返回值

	// combines the results of two parallel computations with a binary function.
	def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

	// as fork is lazy so this is unnecessary 4 unit
	// fork spawns a parallel computation. The computation will not be spawned until forced by run
	def fork[A](a: => Par[A]): Par[A]

	def async[A](a: => A): Par[A] = fork(unit(a))


	type Par[A] = ExecutorService => Future[A]

	def run[A](ex: ExecutorService)(pa: Par[A]): Future[A] = pa(ex)

	// exercise3



	def sum(seq: Seq[Int]): Int = {
		if (seq.size <= 1) seq.headOption getOrElse 0
		else {
			val (l, r) = seq.splitAt(seq.size / 2)

			sum(l) + sum(r)
		}
	}

//	def sumByFuture(seq: Seq[Int]): Int = {
//		if (seq.size <= 1) seq.headOption getOrElse 0
//		else {
//			val (l, r) = seq.splitAt(seq.size / 2)
//
//			Await.result(future(sum(l)), 5 second) +
//			Await.result(future(sum(r)), 5 second)
//		}
//	}

	def sumViaPar(seq: Seq[Int]): Int = {
		if (seq.size <= 1) seq.headOption getOrElse 0
		else {
			val (l, r) = seq.splitAt(seq.size / 2)
			val lPar: Par[Int] = unit(sum(l))  // must begin to execute when starts
			val rPar: Par[Int] = unit(sum(r))

			/*
				if  lPar | aPar does not start , the following + is actually sequential
				get has to wait for the result so it breaks the Parallelization

				Therefore, we must avoid the use of get to return Par[Int]
			*/
			get(lPar) + get(rPar)
		}

		/*
			在书中有这样一句话

			But notice that in this example, if we want to obtain any degree of parallelism, we require that unit begin evaluating its argument immediately
			但是我需要注意的是 如果我们想获取一定程序的并行， 我们需要是参数的计算是立即执行的

			因为Scala表达式是从左到右计算的 所以get(lPar)在get(rPar)之前执行的， 并且要等到第一个执行完之后才能执行第一个 这不就是典型的 顺序性的运算吗

			现在的问题是 虽然unit创造出 异步的计算对象 但是当丢如到get的时候 我们又回到了 逐个执行的 顺序性运算的时候
			所以现在我们要想出一种方法 可以让Par运算同时进行 然后再最后 将所有的结果的汇总
		*/
	}


	/*
		Excercise 1

	*/

	def sumViaRealPar(seq: Seq[Int]): Par[Int] = {
		if (seq.size <= 1) unit(seq.headOption getOrElse 0)
		else {
			val (l, r) = seq.splitAt(seq.size / 2)

			// map2(sum(l), sum(r))(_ + _)
			unit(3)
		}

	}

	def map2 = ???
}
