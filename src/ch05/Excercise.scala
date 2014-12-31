package ch05

/**
 * Created by allen on 14-12-29.
 */
object Excercise extends App {
	val stream = Stream(1, 2, 3, 4)
//	println(stream.toList1)
	println(" take " + stream.take(3).toList)
	println(" takeViaMap " + stream.takeViaFold(3).toList)
//	println(" takeWhile2 " + stream.takeWhile2(_ > 2).toList)
//	println(" takeWhile3 " + stream.takeWhile3(_ > 2).toList)
//	println(" exists " + stream.exists(_ % 14 == 0))
//	println(" takeWhile " + stream.takeWhile(_ > 2).toList)
//	println(" head " + stream.head)
//	println(" tail " + stream.tail.toList)
//	println(" forAll " + stream.forAll(_ >= 1))
//
	println(" map " + stream.map(_ * 3).toList)
//	println(" append " + stream.appendOne(100).toList)
//	println(" flatMap " + stream.flatMap(c => Stream((c to 5).toList: _*)).toList)

	// The execution logic
	// println(" map executed !!! ")
	// println(" filter executed")
	// 交替执行　这样的话针对某些情况 就只需要map一部分 find
	println(stream.map(_ + 10)) // Stream(1, ?).filter(_ > 2)
	println(" mapViaFold " + stream.mapViaFold(_ + 2).toList)

	println(" constantViaFold " + stream.constantViaFold(4).take(4).toList)
	println(" from " +  stream.from(1).take(3).toList)
	println(" from1 generating from unfold" +  stream.fromViaFold(1).take(3).toList)
	println(" fibs " + stream.fibs.take(8).toList)

	println(" fibsViaFold " + stream.fibsViaFold.take(8).toList)
}


