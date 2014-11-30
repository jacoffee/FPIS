package ch03

/**
 * Created by allen on 14-11-30.
 */
trait Animal {
	def speak
}
class Dog(var name: String) extends Animal {
	def speak { println("woof" ) }
	override def toString = name
}
class SuperDog(name: String) extends Dog(name) {
	def useSuperPower { println("Using my superpower!" ) }
}

object InCovariance extends App {
	val fido = new Dog("Fido" )
	val wonderDog = new SuperDog("Wonder Dog" )
	val shaggy = new SuperDog("Shaggy" )

	import collection.mutable.ArrayBuffer
	def makeDogsSpeak(dogs: ArrayBuffer[Dog]) {
		dogs. foreach(_. speak)
	}
	val dogs = new ArrayBuffer[SuperDog]
	//dogs += fido
	dogs += wonderDog
	dogs += shaggy
	//makeDogsSpeak(dogs)
	//makeDogsSpeak(dogs) // can not compile

	/*
		This code won’t compile because of the conflict built up in this situation:
		• Elements in an ArrayBuffer can be mutated.
		• makeDogsSpeak is defined to accept a parameter of type ArrayBuffer[Dog] .
		• You’re attempting to pass in superDogs, whose type is ArrayBuffer[SuperDog] .
		• If the compiler allowed this, makeDogsSpeak could replace SuperDog elements in
		superDogs with plain old Dog elements. This can’t be allowed.
	*/
}
