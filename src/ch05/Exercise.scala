package ch05

/**
 * Created by allen on 14-12-29.
 */
object Exercise extends App {
	val stream = Stream(3, 4, 5, 1, 5, 10, 42)
	println(stream.toList1)
//	println(stream.take(3).toList)
//	println(stream.takeWhile(_ > 2).toList)
//	println(stream.takeWhile2(_ > 2).toList)

//s	println(stream.exists(_ % 14 == 0))
}

