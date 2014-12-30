package ch05

/**
 * Created by allen on 14-12-29.
 */
object Exercise extends App {
	val stream = Stream(3, 1, 5, 1, 5, 10, 42)
	println(stream.toList1)
	println(stream.take(3).toList)
	println(" takeWhile2 " + stream.takeWhile2(_ > 2).toList)
	println(" takeWhile3 " + stream.takeWhile3(_ > 2).toList)
	println(" exists " + stream.exists(_ % 14 == 0))
	println(" takeWhile " + stream.takeWhile(_ > 2).toList)
	println(" head " + stream.head)
	println(" tail " + stream.tail.toList)
	println(" forAll " + stream.forAll(_ >= 1))

	println(" map " + stream.map(_ * 3))
}

