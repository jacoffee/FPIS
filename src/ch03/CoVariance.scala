package ch03

import scala.collection.mutable.ArrayBuffer

/**
 * Created by allen on 14-11-30.
 */
sealed trait Person {
	def speak
}
class Student extends Person {
	def speak  {println(" Student says woof") }
}

class SuperStudent extends Student {
	override def speak  {println(" SuperStudent says woof") }
}

class Container[A](val elem: A)
//object InVariance {
//	def makeDogSpeak(dogs: ArrayBuffer[Student]) = { // if the type is supertype, then subtype can also be passed
//		dogs.foreach(_.speak)
//	}
//	val dogs = new ArrayBuffer[Student]
//	dogs += new Student
//	dogs += new SuperStudent
//
//	makeDogSpeak(dogs)
//}

// Declaring a type as invariant has several effects. First, the container can hold both the specified types as well as its subtypes.
object CoVariance extends App {
	def makeDogSpeak(Student: Container[Student]) = {
		Student.elem.speak
	}
	//val dogs = Seq(new Student, new Student)
	//val superDogs = Seq(new SuperStudent, new SuperStudent)
	makeDogSpeak(new Container(new Student))
	makeDogSpeak(new Container(new SuperStudent))
}


abstract class Transformer[T, R] extends (T => R) {
	def apply(v1: T): R
}