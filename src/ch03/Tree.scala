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

	//  Parant
	//  Branch(left, right)
	def size1[A](tree: Tree[A]): Int = {
		tree match {
			case Leaf(v) => 1
			case Branch(left, right) => 1 + size(left) + size(right)
		}
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

	def maxNum1(tree: Tree[Int]): Int= {
		tree match {
			case Leaf(v) => max(Int.MinValue, v)
			case Branch(left, right) => maxNum1(left) max maxNum1(right)
		}
	}

	// EXERCISE 27: Write a function depth that returns the maximum path length from the root of a tree to any leaf
	def maxDepth[A](tree: Tree[A]): Int = {
		tree match {
			case Leaf(_) => 0
			case Branch(left, right) => 1 + maxDepth(left) + maxDepth(right) // Tree like recursion
		}
	}

	// val newTree = Branch[Int](Leaf(1), Branch[Int](Leaf(3), Branch[Int](Leaf(4), Leaf(56))))
	def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
		// 调用之后 再拼接
		// The following writing lose its original structure, if we want to reuse the originla Structure, we have to recurse over the previous structure
		/*
			def go(tree: Tree[A])(f: A => B): Tree[B]= {
				tree match {
					case Leaf(v) => Leaf(f(v))
					case Branch(left, right) => {
						go(left)(f)
						go(right)(f)
					}
				}
			}
		*/
		tree match {
			case Leaf(v) => Leaf(f(v))
			case Branch(left, right) => Branch(map(left)(f), map(right)(f))
		}
	}

	//  Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively accumulates some value using these handlers.
	// As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and
	// we can use this function to implement just about any recursive function that would otherwise be defined by pattern matching.

	//  Suck and failed version
	// ****************
	// I don't know How to assemble them when applying some sort of logic  to the Data Constructor
	def fold1[A, B](tree: Tree[A], z: B)(f: (A, B) => B): Tree[B] = {
		tree match {
			case Leaf(v) => Leaf(f(v, z))
			case Branch(left, right) => Branch(
				fold1(left, z)(f), fold1(right, z)(f)
			)
		}
	}

	// Answer version
	// f is the handler emerging aforehead, g is the magic to assmeble the changed piece
	def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
		tree match {
			case Leaf(v) => f(v)
			case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
		}
	}

	val newTree = Branch[Int](Leaf(1), Leaf(56))

	// EXERCISE 29: Generalize size, maximum, depth, map writing a new function that abstracts over their similarities.
	// Int => Leaf(5)  max Leaf
	def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(a => 1)(_ + _)

	def maxViaFold(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)

	def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
		fold[A, Tree[B]](tree)(a => Leaf(f(a)))(Branch(_, _))
		//   fold[A, B] -> B的Type是 Tree[B],  而到了 Branch[B] -> 此处的B定义的是data constructor中的类型
	}
}
