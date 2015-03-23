package coursera.week3

/**
 * Created by allen on 3/22/15.
 */
sealed abstract class IntSet {
	def contains(x: Int): Boolean

	def incl(x: Int): IntSet

	def union(intSet: IntSet): IntSet
}

/*
		A
	B       C
  D   E   F   G

*/
case object Empty extends IntSet {
	def contains(x: Int): Boolean = false
	def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty) //Singleton Object

	def union(intSet: IntSet): IntSet = intSet

	override def toString = "."
}

case class NonEmpty(x: Int, left: IntSet, right: IntSet) extends IntSet { self =>
	// binary tree search 2分法查找
	def contains(elem: Int): Boolean = {
		if (elem < x) left contains elem
		else if (elem > x) right contains elem
		else true
	}

	def incl(elem: Int): IntSet = {
		// left | right 可能会有很复杂的结构
		if (elem < x) {
			new NonEmpty(x, left incl elem, right)
		} else if (elem > x) {
			new NonEmpty(x, left, right incl elem)
		} else {
			self
		}
	}

	def union(other: IntSet): IntSet = {
		def go(acc: IntSet, cur: IntSet): IntSet = {
			cur match {
				case NonEmpty(x, left, right) => {
					go(go(acc.incl(x), left), right)
				}
				case Empty => acc
			}
		}
 		go(self, other)
	}

	def unionWithoutRecur(other: IntSet): IntSet =
		((left union right) union other) incl x

	override def toString = "{ " + left + x + right + " }"

	def Linus(param: NonEmpty => IntSet) = {}

	val another = (x: IntSet) => NonEmpty(3, Empty, Empty)

	Linus(another)


}

object IntSetTest extends App {
	val t1 = NonEmpty(10, NonEmpty(5, Empty, Empty), NonEmpty(12, Empty, Empty))
	println(t1)
	val t2 = NonEmpty(12, NonEmpty(6, Empty, Empty), NonEmpty(20, Empty, Empty))
	println(t2)

	println(t1 union t2)
	println(t1 unionWithoutRecur t2 )
}
