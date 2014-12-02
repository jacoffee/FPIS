package ch03

import scala.annotation.tailrec

/**
 * Created by allen on 14-11-30.
 */
object Excercise1 extends App {
	import ch03.List.{ sum, tail, drop, isEmpty, apply, length, foldRight, sum2, product2, length2 , reverse, append, append1, foldLeft, foldLeft1, flatten, map, filter }
	val x = List(1,2,3,4,5) match {
		case Cons(x, Cons(2, Cons(4, _))) => x
		case Nil => 42
		case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
		case Cons(h, t) => h + sum(t)
		case _ => 101
	}
	println(isEmpty(ch03.Nil))
	println(x)
	val sampleList = apply(1, 2, 23, 45, 56,77)
	val sampleList1 = apply(46, 78, 12, 90)
	val concatedList = Cons(sampleList, Cons(sampleList1, Nil))

	println(tail(sampleList))
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
}
