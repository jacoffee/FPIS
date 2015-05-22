package coursera.week3

/*
    algebraic data type A type defined by providing several alternatives, each of which comes with its own constructor.
    It usually comes with a way to decompose the type through pattern matching.
    The concept is found in specification languages and functional programming languages. Algebraic data types can be emulated in Scala with case classes.

    代数类型提供了几种可能性 -> 每个数据结构都有自己的构造器  Empty | NonEmpty

    它一般是用在模式匹配中用于解构数据类型的(一般是抽象父类 或是 特质)

    在Scala中代数数据结构一般是通过case class来模拟

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
RC
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
