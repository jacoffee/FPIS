package ch03

import scala.annotation.tailrec

/**
 * Created by allen on 14-11-30.
 */
object Excercise1 extends App {
	import ch03.List
	import ch03.List.{ sum, tail, drop, isEmpty, apply, length, foldRight, sum2, product2 }
	import ch03.List.{ length2 , reverse, append, append1, foldLeft, foldLeft1, flatten, map, filter, flatMap, filter1, zipWith, hasSubsequence, exists2 }
	import ch03.List.{ dropWhile1, splitAt, take }

	val x = List(1,2,3,4,5) match {
		case ::(x, ::(2, ::(4, _))) => x
		case Nil => 42
		case ::(x, ::(y, ::(3, ::(4, _)))) => x + y
		case ::(h, t) => h + sum(t)
		case _ => 101
	}
	println(isEmpty(ch03.Nil))
	println(x)
	val sampleList = List(1, 2, 23, 45, 56,77)
	val sampleList1 = List(46, 78, 12, 90)
	val concatedList = ::(sampleList, ::(sampleList1, Nil))

	println(exists2(sampleList)(_ % 2 == 0))
	println(" tail " + tail(sampleList))
	println(drop(Nil, 3))
	println(length(sampleList))
	//val stackOverflow = foldRight(apply((1 to 2000000).toList: _*), 0L)(_ + _) // Exception in thread "main" java.lang.StackOverflowError
	println(sum2(sampleList))
	println(product2(sampleList))
	println(length2(sampleList))
	println(" reverse " + reverse(sampleList))
	println(" append " + append(sampleList, 100))
	println(" append1 " + append1(sampleList, 100))
	println(" foldLeft foldRigt")
	println(foldLeft(sampleList, 100)(_ - _))
	println(foldLeft1(sampleList, 100)(_ - _))
	// println(" go 的返回值 " + go(sampleList, sampleList1))
	println(flatten(concatedList))
	println(" map +1" + map(sampleList)(_ + 1))
	val doubleList = apply(1.0, 2.0, 3.0, 4.0)
	println(" map double to String" + map(sampleList)(_ .toString))
	println(" filter " + filter(sampleList)(_ > 50))
	println( " flatMap " + flatMap(sampleList)(i => ::(i, ::(i, Nil))))
	println(" flatMap Test  For " + flatMap(sampleList)(i => List((i,i))))
	println(" filter1 " + filter1(sampleList)(_ > 56))
	println(" zipWith " + zipWith(sampleList, sampleList1)((_, _)))
	println(" hasSubsequences " +  hasSubsequence(sampleList, List(23, 45)) )

	println(" dropWhile1 " + dropWhile1(sampleList)(_ < 24))

	println(" hasSubsequence1 " + hasSubsequence(sampleList, List(1, 2, 23, 45, 56)))

	println(" splitAt " + splitAt(sampleList, 2))
  println(" take " + take(Nil, 2))
}
