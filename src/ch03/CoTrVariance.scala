package ch03

/**
 * Created by allen on 3/3/15.
 */
class GrandParent
class Parent extends GrandParent
class Child extends Parent

class InVaraince[T] {}
class CoVariance[+T] {}
class ContraVariance[-T] {}

object ContraVarianceTest extends App {

	def inCovariantMethod(arg: InVaraince[Parent]) = {}
	def coVariantMethod(arg: CoVariance[Parent]) = {}
	def contraVariantMethod(arg: ContraVariance[Parent]) = {}

	val invariantChild = new InVaraince[Child]
	val invariantParent = new InVaraince[Parent]
	val invariantGrandParent = new InVaraince[GrandParent]

	val covariantChild = new CoVariance[Child]
	val covariantParent = new CoVariance[Parent]
	val covariantGrandParent = new CoVariance[GrandParent]

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
}


trait OutputChannel[-T] {
	def write(x: T)
}