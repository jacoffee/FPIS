package ch03

/**
 * Created by allen on 14-12-3.
 */

object Excercise2 extends App {
	import ch03.Tree.{ sizeByGo, maxNum, map, size, mapViaFold }
	val newTree = Branch[Int](Leaf(1), Branch[Int](Leaf(3), Branch[Int](Leaf(4), Leaf(56))))
	println(" size " + sizeByGo(newTree))
	println(" size1 " + size(newTree))
	println(" maxNum " + maxNum(newTree))
	println(" maxNum1  " + maxNum(newTree))
	println(" map " + map(newTree)(_ + 1))

	println(" mapViaFold " + mapViaFold(newTree)(_ * 3))
}