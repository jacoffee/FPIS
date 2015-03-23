package ch03

/**
 * Created by allen on 3/8/15.
 */
abstract class VarianceAnnotation[B, A <: B, C >:A <:B] {
	// rule1
	def m(x: C) = ""
}

class CP[+T] {}
class C[+T] extends CP[T] {}

abstract class Q[+A, +B](x: A, y: B) {
	//var fst: A = x // **** error: illegal variance:
	//var snd: B = y // ‘A’, ‘B’ occur in invariant position.
}
//
//abstract class Cat[-T, +U] {
//	def meow[T](volume: T, listener: Cat[U, T]): Cat[Cat[U, T], U]
//}


class Foo[A, B] {
	def foo(a : A) = {
		def bar(b : B) = ???
	}
}