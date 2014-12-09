package ch03

/**
 * Created by allen on 14-12-9.
 */

class Publication(val title: String)
class Book(title: String) extends Publication(title)
// case class Book(title: String) extends Publication(title) will notice title needs override modifier

object Library {
	val books: Set[Book] = {
		Set(
			new Book("Scala CookBook"),
			new Book("Maven in Action")
		)
	}

	// class Fuction1[-T, +U]
	// cause  Book => AnyRef ------> new Function1[Book, AnyRef] {}
	// Book is in the negative position, so Book and its super type can be accepted
	// AnyRef is in the positive position, so AnyRef's (Object)'s sub type can be passed
	def printBookList(info: Book => AnyRef) = {
		for { book <- books } { println(info(book)) }
	}
}

object Customer extends App {
	def getTitle(p: Publication): String = p.title
	Library.printBookList(getTitle)
}
