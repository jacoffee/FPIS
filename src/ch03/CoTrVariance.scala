package ch03

/**
 * Created by allen on 3/3/15.
 */
class GrandParent
class Parent extends GrandParent
class Child extends Parent

class InVaraince[T] {}
class CoVariance1[+T] {
	/*
		The variance position of a method parameter is the opposite of the variance position of the enclosing parameter clause.
		方法参数所处位置的类型变化的方向
	*/
}
class ContraVariance[-T] {
	def apply(x: T) = ""
}

object ContraVarianceTest extends App {

	def inCovariantMethod(arg: InVaraince[Parent]) = {}
	def coVariantMethod(arg: CoVariance1[Parent]) = {}
	def contraVariantMethod(arg: ContraVariance[Parent]) = {}

	val invariantChild = new InVaraince[Child]
	val invariantParent = new InVaraince[Parent]
	val invariantGrandParent = new InVaraince[GrandParent]

	val covariantChild = new CoVariance1[Child]
	val covariantParent = new CoVariance1[Parent]
	val covariantGrandParent = new CoVariance1[GrandParent]

	val contraVariantChild = new ContraVariance[Child]
	val contraVariantParent = new ContraVariance[Parent]
	val contraVariantGrandParent = new ContraVariance[GrandParent]

//	inCovariantMethod(invariantChild) // 编译不通过
//	inCovariantMethod(invariantParent) // 编译通过
//	inCovariantMethod(invariantGrandParent) // 编译不通过

	coVariantMethod(covariantChild) // 编译通过
	coVariantMethod(covariantParent) // 编译通过
//	coVariantMethod(covariantGrandParent) // 编译不通过

//	contraVariantMethod(contraVariantChild) // 编译不通过
	contraVariantMethod(contraVariantParent) // 编译通过
	contraVariantMethod(contraVariantGrandParent) // 编译通过


	def variantFunc(arg: Parent => AnyRef) = {}

	def variantFunc1(arg: Parent): Parent = new Parent
	variantFunc1(new Child)

	val pp = (x: Parent) => new Parent // Function1[Parent, Parent]
	val gp = (x: GrandParent) => new Parent // Function1[GrandParent, Parent]
	val gc = (x: GrandParent) => new Child // Function1[GrandParent, Child]

	variantFunc(pp)
	variantFunc(gp)
	variantFunc(gc)

	val o1 = new OutputChannel[String] {
		def write(x: String) = Console.println(x)
	}

	val o2 = new OutputChannel[AnyRef] {
		def write(x: AnyRef) = Console.println(x)
	}

	// Container[Parent] with Object { def ts1[Parent](x: String): Unit }
	val child = new Container[String] {
		def ts1[Any](x: Any) = "valid"
	}

	val parent: Container[Any] = child
	parent.ts1(new Parent)

}

trait Container[+A] {
	// def ts[B >: A]

	/*
		The variance position of the lower bound of a type declaration or typeparameter
		is the opposite of the variance position of the type declaration or parameter
		下界类型的VP和它定义类型的VP是相反的

	*/
	// def ts1[B >: A](x: B)
	//def ts2[B <: A](x: B)
	def ts1[B >: A](x: B)
	// def ts2[B <: A](x: B): B
}

//trait Container[-A] {
//	def ts3[B <: A](x: B): B // successful compilation
//
//	def ts4[B >: A](x: B): B
//}

abstract class PrivateVar[+A, -C, B[C]](x: A) {
	protected[this] var ts2: A = x

	/*
	 Error covariant type C occurs in invariant position
	 in type => B[C] of method ts3
	*/
	//var ts3: A
}


