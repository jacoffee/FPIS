package ch09

import scala.util.Try

/**
 * Created by allen on 5/28/15.
 */

// just like trait Parsers[A, [+_]]
trait Parsers[ParseError, Parser[+_]] { self =>

	// type ParseError[T] = Try[T]

	/*
		无论接下来，要如何展开，首先我们要明白Parser的数据结构
		就像Par[A] = (e: ExecutorService) => Future[A]
	*/

	def char(A: Char): Parser[Char]

	// abra
	def string(A: String): Parser[String]

	// abra cadabra
	def orString(A: String, B: String): Parser[String]

	// 但是String显然还是比较低得数据结构 继续抽象
	def orParser(A: Parser[String], B: Parser[String]): Parser[String]

	// 再进一步 类型化
	def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

	/*
		上面是典型的or 操作 所以马上会想到 自定义 operation

		or => ||
		and  &&
	*/
	def run[A](p: Parser[A])(input: String): Either[ParseError, A]

	implicit def stringToParser(s: String): Parser[String]

	implicit def operators[A](p: Parser[A]) = ParserOp(p)

	// String类型的Op
	implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOp[String] =
		ParserOp(f(a))

	case class ParserOp[A](p: Parser[A]) {
		def ||[B >: A](that: Parser[B]): Parser[B] = self.or(p, that)
	}
}
