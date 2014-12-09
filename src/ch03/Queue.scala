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
trait MyQueue[+T] { // default -> nonvariant  +T makes it variant
	def head: T
	def tail: MyQueue[T]
	def append[U >: T](u: U): MyQueue[U]
	def isEmpty: Boolean
	def foreach[B](f: T => B): Unit
	// this position is negative casue in implemetation hierarchy, something can be done in subtype can not be done in supertype
	// if MyQueue is defined as Covaraint, it means more flexible, its subtype can be passed into the append method
	// however method works on T, does not apply to its subtype
	// Therefore the compiler warning is absolutly right

	// The trick is lower bound
	// In order to prevent subtype of T be passed into the append method, we make it as the lower bound
	/*
		Error:(40, 13) covariant type T occurs in contravariant position in type T of value t
		def append(t: T): MyQueue[T]
				^
		Reassignable fields are a special case of the rule that disallows type parameters annotated with + from being used as method parameter types.
	*/
}

object MyQueue {

	def apply[T](ts: T*): MyQueue[T] = new MyQueueImpl(ts.toList, SNil)

	private class MyQueueImpl[+T](
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

		def append[U >: T](u: U): MyQueue[U] = new MyQueueImpl(leading, u :: tailing)

		def isEmpty = leading.isEmpty && tailing.isEmpty
		def foreach[B](f: T => B) {
			(leading ::: tailing.reverse).foreach(f)
		}
	}
}

class Cell[T](init: T) {
	private[this] var current = init
	def get = init
	def set(t: T): Unit = {
		current = t
	}
}


object Test extends App {
	def doesNotCompile(q: MyQueue[AnyRef]) = {} // class Queue takes type parameters
	// Queue is a trait, but not a type. Queue is not a type because it takes a type parameter.
	//class Fruit {}
	class Apple(val name: String)
	class Orange(val name: String)

	//def cotra(q: MyQueue[Apple]) = {}

	val stringQueue = MyQueue("nihao", "enqueue", "fast killing")
	doesNotCompile(stringQueue)

	val fruitQ = MyQueue(new Orange("131"))
	val cc = fruitQ.append(new Orange("12"))
	fruitQ.append(new Apple("13")) // MyQueue[Object] cause U >: Orange, Object is the supertype of
	println(" fruitQ ")
	fruitQ.foreach { o =>
		println(o.name)
	}

	val c1 = new Cell[String] ("abc")
//	val c2: Cell[Any] = c1
//	c2.set(1)
//	val s: String = c1.get

}

