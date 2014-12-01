package ch03

/**
 * Created by allen on 14-11-30.
 */
object Excercise1 extends App {
	import ch03.List.{ sum, tail, drop, isEmpty, apply, length, foldRight }
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
	println(tail(sampleList))
	println(drop(Nil, 3))
	println(length(sampleList))
	//val stackOverflow = foldRight(apply((1 to 2000000).toList: _*), 0L)(_ + _) // Exception in thread "main" java.lang.StackOverflowError
}
