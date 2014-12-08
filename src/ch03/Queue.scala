package ch03

/**
 * Created by allen on 14-12-8.
 */
import  scala.collection.immutable.{ List => SList, Nil => SNil }

// class Queue[+T] private  --->  it can be accessed only from within the class itself and its companion object
class Queue[+T] private(
	private val leading: SList[T],
	private val tailing: SList[T]
) {
	def mirror = {
		if (leading.isEmpty) {
			new Queue(tailing.reverse, SNil)
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

// One Way to hide the implementation of Queue
// new Queue(List(1,2,3), List(4, 5,6)) => Queue(List(1,2,3,4,5,6))
object Queue {
	def apply[T](ts: T*) = new Queue(ts.toList, SNil)
}

// Another Way of doing so
trait MyQueue[T] {
	def head: T
	def tail: MyQueue[T]
	def append(t: T): MyQueue[T]
}

object MyQueue {

	def apply[T](ts: T*): MyQueue[T] = new MyQueueImpl(ts.toList, SNil)

	private class MyQueueImpl[T](
		private val leading: SList[T],
		private val tailing: SList[T]
	) extends MyQueue[T] {
		def mirror = {
			if (leading.isEmpty) {
				new MyQueueImpl(tailing.reverse, SNil)
			} else this
		}

		def head = mirror.leading.head

		def tail: MyQueueImpl[T] = {
			val q = mirror
			new MyQueueImpl(q.leading.tail, q.tailing)
		}

		// [T] 不写会有问题d
		def append(t: T): MyQueue[T] = new MyQueueImpl(leading, t :: tailing)
	}
}

