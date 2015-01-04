package ch05

/**
 * Created by allen on 14-12-29.
 */
object Excercise extends App {
	val stream = Stream(1, 2, 3, 4, 10)
	val stream2 = Stream(1, 2, 3)
	println(stream.toList1)
	println(" take " + stream.take(3).toList)
	println(" takeViaFold " + stream.takeViaFold(3).toList)
	println(" takeWhile2 " + stream.takeWhile2(_ > 2).toList)
	println(" takeWhile3 " + stream.takeWhile3(_ > 2).toList)
	println(" exists " + stream.exists(_ % 14 == 0))
	println(" takeWhile " + stream.takeWhile(_ > 2).toList)
	println(" head " + stream.head)
	println(" tail " + stream.tail.toList)
	println(" forAll " + stream.forAll(_ >= 1))

	println(" map " + stream.map(_ * 3).toList)
	println(" append " + stream.appendOne(100).toList)
	println(" flatMap " + stream.flatMap(c => Stream((c to 5).toList: _*)).toList)

	// The execution logic
	println(" map executed !!! ")
	println(" filter executed")
	//交替执行　这样的话针对某些情况 就只需要map一部分 find
	println(stream.map(_ + 10)) // Stream(1, ?).filter(_ > 2)
	println(" mapViaFold " + stream.mapViaFold(_ + 2).toList)

	println(" constantViaFold " + stream.constantViaFold(4).take(4).toList)
	println(" from " +  stream.from(1).take(3).toList)
	println(" from1 generating from unfold" +  stream.fromViaFold(1).take(3).toList)
	println(" fibs " + stream.fibs.take(8).toList)

	println(" fibsViaFold " + stream.fibsViaFold.take(8).toList)

	println(" zipWith " + stream.zipWith(stream2)(_ * _).toList)
	println(" zipWithViaFold " + stream.zipWithViaFold(stream2)(_ * _).toList)


	println(" zipWithAll " + stream.zipWithAll[Int, Int](stream2)(
		(first, second) => {
			(first, second) match {
				case (Some(a), Some(b)) => a * b
				case (Some(a), None) => a
				case (None, Some(b)) => b
				case (None, None) => 1
			}
		}
	).toList)

	println(" startsWith " + stream.startsWith(Stream(1, 2, 3, 4, 10, 11, 12)))
	println(" tails " + stream.tails.map(_.toList).toList)
	println(" hasSubsequence " + stream.hasSubsequence(Empty))
	println(" Stream Equals " + stream == Stream(1, 2, 3, 4, 10))
	println(" === " + stream.===(Stream(1, 2, 3, 4, 10)) ) // 为什么stream === Stream..

	println(" scanRight " + Stream(1, 2, 3).scanRight(0)(_ + _).toList)

	println(" scanRightViaFoldRight " + Stream(1, 2, 3).scanRightViaFoldRight(0)(_ + _).toList)
}


