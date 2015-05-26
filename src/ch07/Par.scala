package ch07

import java.util.concurrent.{ ForkJoinPool, ExecutorService }

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await

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


	/*
		We could of course run the Par, sort the resulting list, and re-package it in a Par with unit. But we want to avoid calling run
		这个地方跟我开始做sequence的时候心态是一样的，计算Par[List[Int]] 抽取出 List[Int], sort 然后再使用unit进行封装
	*/
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
	// just like the original method new Thread start
	def fork[A](a: => Par[A]): Par[A] = {
		es => es.submit(new Callable[A] {
			def call(): A = a(es).get
		})
	}

	/*
		普通的map的操作会对每一个元素调用函数，如果是很容易就完成的函数，那么新开一个线程去做没有很大的意思
		但是如果是个比较费时的操作，那么这种操作就会非常有意义
		首先，它避免了blocking 因为
		sequential computation requires the completion of previous operation which may get stuck
		其次，加速了整个过程，同时进行多项复杂的运算

		Summing integers is in practice probably so fast that parallelization imposes mroe overhead that is saves
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

	/*
		Write a function that takes a list of paragraphs (a List[String]), and returns the total number of words across all paragraphs, in parallel.
		Generalize this function as much as possible

		Implement map3, map4, and map5, in terms of map2.
	*/
	def map3[A,B,C,D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
		// def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
		???
	}

	def getWordCount(l: List[String]) = {
		map(parMap(l)(_.split("\\s+").size))(_.sum)
	}

	val result = run(getWordCount(List("abc edf", "ghy iju lki"))).get

	map(unit(1))(_ + 1) == unit(2)

	/*
		how to judge that two pars are the same if
		given same executor service, they return results in the same value
	*/
	def equals[A](p1: Par[A], p2: Par[A])(implicit ex: ExecutorService) = {
		p1(ex).get == p2(ex).get
	}

	def id[A](a: A): A = a
}

trait Par1[+A] {
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

	def sumViaRealPar(seq: Seq[Int]): Par[Int] = {
		if (seq.size <= 1) unit(seq.headOption getOrElse 0)
		else {
			val (l, r) = seq.splitAt(seq.size / 2)

			// map2(sum(l), sum(r))(_ + _)
			unit(3)
		}

	}
}
