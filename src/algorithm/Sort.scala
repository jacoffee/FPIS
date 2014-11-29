package algorithm

import scala.util.Random


/**
 * Created by allen on 14-11-29.
 */
object Sort extends App {
	val arr = Array(1, 2, 3, 4)
	arr(1) > arr(2)

	// def bubbleSort[T](arr: Array[T])(implicit cmp: Ordering[T]) = {}
	def swap(index1: Int, index2:Int) = {
	}

	var count = 0
	def bubbleSort(arr: Array[Int]) = {
		val start = System.currentTimeMillis
		val len = arr.size
		var temp = 0
		for (i <- 0 to (len -1)) {
			for (j <- (i+1) to (len-1)) {
				if (arr(j) < arr(i)) { // 每一次都要和自己后面的进行比较
					count = count +1
					temp = arr(i)
					arr(i) = arr(j)
					arr(j) = temp
				}
			}
		}
		println(" bubbleSort 用时 " + (System.currentTimeMillis -start))
		arr
	}

	var count1 = 0
	// 把i当成比较的轮次
	def betterBubbleSort(arr: Array[Int]) = {
		val start = System.currentTimeMillis
		val len = arr.size
		var temp = 0
		for (i <- 1 to len -1) {
			for (j <- len -1 to i by -1) {
				if  (arr(j-1) >  arr(j)) {
					count1 = count1 +1
					temp = arr(j)
					arr(j) = arr(j-1)
					arr(j-1) = temp
				}
			}
		}
		println(" betterBubbleSort 用时 " + (System.currentTimeMillis -start))
		arr
	}

	val testArr = Random.shuffle((1 to 100000).toList)
	//bubbleSort(testArr.toArray)
	bubbleSort(testArr.toArray)
	println(" 次数 " + count)  // 5*4/2
	betterBubbleSort(testArr.toArray)
	println(" 次数1 " + count1)  // 5*4/2
}
