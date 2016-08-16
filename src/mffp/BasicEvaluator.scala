package mffp

sealed trait Term
case class Con(i: Int) extends Term
case class Div(numerator: Term, denominator: Term) extends Term

// Monad的作用就要通过某种 结构让下面几种结构的计算更加容易 ==> abstraction over computation
object BasicEvaluator {
  def eval: Term => Int =
    (term: Term) =>
       term match {
         case Con(x) => x
         case Div(term1, term2) =>
           eval(term1) / eval(term2)
       }

   val answer = Div(Div(Con(1972), Con(2)), Con(23)) // 1972 / 2 / 23s
   val error = Div(Con(1), Con(0)) // ** Exception: divide by zero -- side effects

   eval(answer)
   eval(error)
}


// TODO ==> 每次计算的时候 需要进行异常检验

// M represented computation that raise exception

// 添加对于异常的处理, 在Scala实际上有更好的实现Either只不过为了遵从原文的思路采用下面的方式
sealed trait Maybe[+A]
case class Raise(excep: String) extends Maybe[Nothing]
case class Value[A](x: A) extends Maybe[A]

object BasicEvaluatorWithExcep {

  def eval: Term => Maybe[Int] =
    (term: Term) =>
      term match {
        case Con(x) => Value(x)
        case Div(term1, term2) =>
          // 每一步都需要做异常判断
          eval(term1) match {
            case raise1 @ Raise(_) => raise1
            case Value(v1) =>
              eval(term2) match {
                case raise2 @ Raise(_) => raise2
                case Value(v2) =>
                  if (v2 == 0) {
                    Raise("divide by zero")
                  } else Value(v1 / v2)
              }
          }
      }

  val answer = Div(Div(Con(1972), Con(2)), Con(23)) // 1972 / 2 / 23s
  val error = Div(Con(1), Con(0)) // ** Exception: divide by zero -- side effects

  eval(answer)
  eval(error)
}


// 状态: 使用一个Count来计算次数 -- count the number of divisions perforamed during evaluation
// TODO ==> 每次调用的时候 需要传入老的状态
// 使用一个类型来表示 这个状态

// M represented computation that act on state ===> Stare Monad
object BasicEvaluatorWithState {

    // type alias -- type synonym
    type State = Int
    type M[A] = Int => (A, Int) // 接受一个开始时, <<除过的次数>> 计算完成之后 返回<<计算的值>>以及完成之后的<<除过的次数>>

    def eval: Term => M[Int] = {
      (term: Term) => {
          term match {
            case Con(x) =>
              time: Int => (x, time) // 如果常数A, 就没有所谓的除数过程, 所以传入的次数就最终的次数

            case Div(term1, term2) =>
              (time: Int) => {
                val (a, t1) = eval(term1)(time)
                val (b, t2) = eval(term2)(t1)

                // M a is a function that accepts the initial state, and returns the computed value paired with the final state
                (a / b, t2 + 1)
              }
          }
      }
    }
}

// Log.info 追踪程序的执行 也就是我们常用的println, 也就是FP中经常提到的side effect

// TODO ==> 使用一个类型来表示输出, 然后使用某种Monadic Operation来 concatenate

// M represented computation that generate Output ===> IO Monad

object BasicEvaluatorWithOutput {
  type Output = String
  type M[A] = (List[Output], A)

  // 参与运算的Term, 计算结构转换成Output, 没运算一次生成一个Output
  // line :: Term -> Int -> Output
  def line: (Term, Int) => List[Output] = (term, i) => s"eval $term <= $i" :: Nil

  def eval: Term => M[Int] = {
    (term: Term) => {

      term match {
        case con@Con(x) =>
          (line(con, x), x)

        case div@Div(term1, term2) =>
          val (output1, a) = eval(term1)
          val (output2, b) = eval(term2)

          // 这种思想求RedBlackTree的高度时 遇到过 max(height(l), height(r))
          (output1 ::: output2 ::: line(div, a / b), a / b)
      }
    }
  }

  val answer = Div(Div(Con(1972), Con(2)), Con(23)) // 1972 / 2 / 23s
  val error = Div(Con(1), Con(0)) // ** Exception: divide by zero -- side effects

  eval(answer)._1.foreach(println)
}


// Monadic Implementation

/*
 上面的情况下, 最初的function type: A => B

  接下来的类型都是 A => M[B], M represented some sort of computation

  1> then the first question pops up in your mind may be -- how can we convert B to M[B]

  lots of the Ancestors || predecessors has conclude the one for us i.e unit or point in Scalaz

  unit :: a -> M a

  所以上面的Con实际上就是 unit操作

  2>  we have M a, we encounter other forms of data computation a -> M[B] -- how can we convert M[A] to M[B]

  >>= :: M a -> (a -> M b) -> M b === m * (λa).n

  A monad is a triple of (M, unit, >>=), M is type constructor & two monadic combinator

*/


// restrict A to Int
trait Monad[M[_]] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(fab: A => M[B]): M[B]
}

object MonadAbstraction {

  val evaluatorMonad = new Monad[Maybe] {

    def eval: Term => Maybe[Int] = {
      (term: Term) => {
        term match {
          case Con(x) => unit(x)
          case Div(term1, term2) =>
            flatMap(eval(term1)) { v1 =>
              flatMap(eval(term2)) { v2 =>
                unit(v1 / v2)
              }
            }
        }
      }
    }

    override def unit[A](a: => A): Maybe[A] = Value(a)

    override def flatMap[A, B](ma: Maybe[A])(fab: (A) => Maybe[B]): Maybe[B] = {
      ma match {
        case Value(x) => fab(x)
        case r @ Raise(raise) => r
      }
    }
  }

  type Output = List[String]
  type M[A] = (Output, A)

  val ioMonad = new Monad[M] {
    override def unit[A](a: => A): (Output, A) = (Nil, a)
    override def flatMap[A, B](ma: (Output, A))(fab: (A) => (Output, B)): (Output, B) = {
      val (o1, i1) = ma
      val (o2, i2) = fab(i1)
      (o1 ::: o2, i2)
    }

    def line: (Term, Int) => Output = (term, i) => s"eval $term <= $i" :: Nil

    def write(out: => Output) = (out, ())

    def eval: Term => M[Int] = {
      (term: Term) => {
        term match {
          case con @ Con(x) =>
            //如何把unit引入进来, 所以原论文中引入了 Output --- M [Int]
            // (line(con, x), x) 并没有体现Monadic Composition的思想

            /*
               原理的实现过程中 直接将最后的结果拼接出来, 按照Monad的思想, 这些结果可以通过不同的计算流程来实现
                write 部分相当于原来的 输出, 而 函数部分就是原来的Value 那一部分通过 composition组合起来了

                类似于Id[A]
                for {
                   a <- Id("Helle ")
                   b <- Id(" World)
                } yield { a + b }

                与
                "Hello " + " World"

                正是由于composition的思想, 所以函数依次被执行最终得到结果, 也使得Lazy Computation成为可能

                the impure display output as it computes, while pure version display nothing until the entire computation completes
           */
            flatMap(write(line(con, x))) { o => unit(x)}
          case div @ Div(term1, term2) =>
            flatMap(eval(term1)) { a =>
              flatMap(eval(term2)) { b =>
                // (line(div, a / b), a / b)
                flatMap(write(line(div, a / b))) { o =>
                  unit(a / b)
                }
              }
            }
        }
      }
    }

  }
}

