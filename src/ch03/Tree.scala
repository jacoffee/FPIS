package ch03

import scala.collection.immutable.List

/**
 * Created by allen on 14-12-3.
 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], Right: Tree[A]) extends Tree[A]

object Tree {
	def size[A](tree: Tree[A]) = {
		var count = 0
		tree match {
			case Leaf(_) =>
			case Branch(left, right) =>
		}
	}
}
