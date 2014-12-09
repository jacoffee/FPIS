package ch03

/**
 * Created by allen on 14-12-9.
 */
trait OutputChannel[-T] {
	def write(t: T)
}

class Vegetable {}
class Carot extends Vegetable {}

object Cotravariance extends App {
	// Cotravariance comes from the fact that in this case, OutputChannel[AnyRef] is a subtype of OutputChannel[String]
	// while the generic type String is a subtype of AnyRef

	def write(t: OutputChannel[String]) = println("xxxxxxxxx")
	val anyMy = new OutputChannel[AnyRef] {
		def write(t: AnyRef) = println(" trait and type are two different things")
	}
	write(anyMy)
}
