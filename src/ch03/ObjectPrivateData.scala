package ch03

/**
 * Created by allen on 14-12-10.
 */
import  scala.collection.immutable.{ List => SList, Nil => SNil }

object ObjectPrivateData {

}

object NewQueue {
	def apply[T](xs: T*) = new NewQueue(xs.toList, SNil)

	val qq = NewQueue(1,2, 3, 4)
}

class NewQueue[+T] (
	private[this] var leading: SList[T],
	// private var leading: SList[T] Error:(13, 7) covariant type T occurs in contravariant position in type List[T] of value leading_=
	private[this] var trailing: SList[T]
	// private[this]
)  {
	private def mirror = {
		if (leading.isEmpty) {
			while(trailing.nonEmpty) {
				leading = trailing.head :: leading
				trailing = trailing.tail
			}
		}
	}

	def head = {
		mirror
		leading.head
	}

	def tail: NewQueue[T] = {
		mirror
		new NewQueue[T](leading.tail, trailing)
	}

	def append[U >: T](u: U): NewQueue[U] = new NewQueue[U](leading, u :: trailing)

}

