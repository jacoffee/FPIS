package ch04

import ch03.List

object Excercise extends App {
	// def sequence[A](a: List[Option[A]]): Option[List[A]] = {
	val optionList = List(Some("だいごく"), Some("ぎんこう"), Some("まあいさ"))
	val result1 = Option.sequence(optionList)
	println(" Excercise5 " + result1)

}
