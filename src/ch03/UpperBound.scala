package ch03

/**
 * Created by allen on 14-12-10.
 */
//import scala.collection.immutable.{ List => SList, Nil => SNil }
import scala.runtime.RichInt

object UpperBound {
	val people = List(
		new People("Larry", "Wall"),
		new People("Anders", "Hejlsberg"),
		new People("Guido", "van Rossum"),
		new People("Alan", "Kay"),
		new People("Yukihiro", "Matsumoto")
	)

	orderedMergeSort(people)
	// TODO Left undone
	// orderedMergeSort(List(1,2,3).map(intWrapper))

	def orderedMergeSort[T <: Ordered[T] ](xs:List[T] ): List[T] = {
		// split into two pieces, compare the first element of the two collections
		def merge(smallPart: List[T], biggerPart: List[T]): List[T] = {
			(smallPart, biggerPart) match {
				case (Nil, _) => biggerPart
				case (_, Nil) => smallPart
				case (h1 :: t1, h2 :: t2) => {
					if (h1 < h2) h1 :: merge(t1, biggerPart)
					else h2 :: merge(smallPart, t2)
				}
			}
		}

		val n = List.size(xs) / 2
		if (n == 0) xs
		else {
			val (head, tail) = List.splitAt(xs, n)
			merge(orderedMergeSort(head), orderedMergeSort(tail))
		}
	}
}

class People(val firstName: String, val lastName: String) extends Ordered[People] {
	def compare(that: People) = {
		val lastNameComparison =
			lastName.compareToIgnoreCase(that.lastName)
		if (lastNameComparison != 0)
			lastNameComparison
		else
			firstName.compareToIgnoreCase(that.firstName)
	}

	override def toString = firstName +" "+ lastName
}
