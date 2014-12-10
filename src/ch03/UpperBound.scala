package ch03

/**
 * Created by allen on 14-12-10.
 */
import scala.collection.immutable.{ List => SList, Nil => SNil }
import scala.runtime.RichInt

object UpperBound {
	val people = SList(
		new People("Larry", "Wall"),
		new People("Anders", "Hejlsberg"),
		new People("Guido", "van Rossum"),
		new People("Alan", "Kay"),
		new People("Yukihiro", "Matsumoto")
	)

	orderedMergeSort(people)
	orderedMergeSort(
		SList(1,2,3).map { num =>
			new RichInt(num)
		}
	)

	def orderedMergeSort[T <: Ordered[T] ](xs:SList[T] ): SList[T] = {
		// split into two pieces, compare the first element of the two collections
		def merge(smallPart: SList[T], biggerPart: SList[T]): SList[T] = {
			(smallPart, biggerPart) match {
				case (SNil, _) => biggerPart
				case (_, SNil) => smallPart
				case (h1 :: t1, h2 :: t2) => {
					if (h1 < h2) h1 :: merge(t1, biggerPart)
					else h2 :: merge(smallPart, t2)
				}
			}
		}

		val n = xs.size / 2
		if (n == 0) xs
		else {
			val (head, tail) = xs splitAt n
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
