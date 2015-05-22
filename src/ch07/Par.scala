package ch07

import scala.concurrent.{ Await, future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
 * Created by allen on 1/17/15.
 */


/*
	sequential computing - blocking
	parallelized
*/
trait Par[+A] {
	def unit[A](a: => A): Par[A] // 生成并行运算

	def get[A](a: Par[A]): A // 执行并行运算并收集返回值

	def sum(seq: Seq[Int]): Int = {
		if (seq.size <= 1) seq.headOption getOrElse 0
		else {
			val (l, r) = seq.splitAt(seq.size / 2)

			sum(l) + sum(r)
		}
	}

	def sumByFuture(seq: Seq[Int]): Int = {
		if (seq.size <= 1) seq.headOption getOrElse 0
		else {
			val (l, r) = seq.splitAt(seq.size / 2)

			Await.result(future(sum(l)), 5 second) +
			Await.result(future(sum(r)), 5 second)
		}
	}

	def sumViaPar(seq: Seq[Int]): Int = {
		if (seq.size <= 1) seq.headOption getOrElse 0
		else {
			val (l, r) = seq.splitAt(seq.size / 2)
			val lPar: Par[Int] = unit(sum(l))
			val rPar: Par[Int] = unit(sum(r))
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
}
