package ch03

/**
 * Created by allen on 14-12-3.
 */

object Excercise2 extends App {
	import ch03.Tree.{ size, maxNum, map }
	val newTree = Branch[Int](Leaf(1), Branch[Int](Leaf(3), Branch[Int](Leaf(4), Leaf(56))))
	println(" size " + size(newTree))
	println(" maxNum " + maxNum(newTree))
	println(" map " + map(newTree)(_ + 1))
}