package ch03

/**
 * Created by allen on 14-12-8.
 */
import  scala.collection.immutable.{ List => SList }

class Queue[+T] (
	private val leading: SList[T],
	private val tailing: SList[T]
) {
	def mirror = {
		if (leading.isEmpty) {
			new Queue(leading, tailing.reverse)
		} else this
	}

	def head = mirror.leading.head

	def tail = {
		val q = mirror
		new Queue(q.leading.tail, tailing.reverse)
	}

	// [T] 不写会有问题d
	def append[T](t: T) = new Queue(leading, t :: tailing)
}
