package ch03

/**
 * Created by allen on 14-12-3.
 */

object Excercise2 extends App {
	import ch03.Tree.{ size, maxNum, map, size1, maxNum1, size3 }
	val newTree = Branch[Int](Leaf(1), Branch[Int](Leaf(3), Branch[Int](Leaf(4), Leaf(56))))
	println(" size " + size(newTree))
	println(" size1 " + size1(newTree))
	println(" maxNum " + maxNum(newTree))
	println(" maxNum1  " + maxNum1(newTree))
	println(" map " + map(newTree)(_ + 1))


	println(" size3 " +  size3(newTree))
}