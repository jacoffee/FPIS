package ch03

import scala.annotation.tailrec
import scala.math.max

/**
 * Created by allen on 14-12-3.
 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	// ADT and Pattern Match

	// EXERCISE 25: Write a function size that counts the number of nodes in a tree.
	def size[A](tree: Tree[A]) = {
		var count = 0
		def go(tree: Tree[A]): Int= {
			tree match {
				case Leaf(_) => {
					count = count + 1
					count
				}
				case Branch(left, right) => {
					go(left)
					go(right)
				}
			}
		}
		go(tree)
	}

	// EXERCISE 26: Write a function that maximum returns the maximum element in a Tree[Int]。
	def maxNum(t: Tree[Int]): Int= {
		import scala.math.max
		def go(tree: Tree[Int]): Int= {
			tree match {
				case Leaf(v) => max(Int.MinValue, v)
				case Branch(left, right) => {
					go(left)
					go(right)
				}
			}
		}
		go(t)
	}

	// EXERCISE 27: Write a function depth that returns the maximum path length from the root of a tree to any leaf
	def maxDepth[A](t: Tree[A]): Int = {
		3
	}

	// val newTree = Branch[Int](Leaf(1), Branch[Int](Leaf(3), Branch[Int](Leaf(4), Leaf(56))))
	def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
		// 调用之后 再拼接
		// The following writing lose its original structure, if we want to reuse the originla Structure, we have to recurse over the previous structure
		def go(tree: Tree[A])(f: A => B): Tree[B]= {
			tree match {
				case Leaf(v) => Leaf(f(v))
				case Branch(left, right) => {
					go(left)(f)
					go(right)(f)
				}
			}
		}
		t match {
			case Leaf(v) => Leaf(f(v))
			case Branch(left, right) => Branch(map(left)(f), map(right)(f))
		}

	}
}
