package ch10

/**
 * Created by allen on 5/27/15.
 */
trait TraitTest {

}

abstract class Table[A, B](defaultValue: B) {

	def get(key: A): Option[B]
	def set(key: A, value: B)
	def apply(key: A) = get(key) match {
		case Some(v) => v
		case None => defaultValue
	}
}

class ListTable[A, B](defaultValue: B) extends Table[A, B](defaultValue) {
	private var elems: List[(A, B)] = Nil
	def get(key: A) = elems.find(_ == key).map(_._2)
	def set(key: A, value: B) = { elems = (key, value) :: elems }
}

object MyTable extends ListTable[String, Int](0)