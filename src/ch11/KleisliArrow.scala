package ch11

import java.io.File
import scala.io.Source
import scalaz._
import Scalaz._
import Kleisli.kleisli

object KleisliArrow {

  val files: String => List[File] = (dir: String) => new File(dir).listFiles.toList

   // number of words
   val wordCalculator: File => List[Int] =
     (file: File) =>
       Source.fromFile(file, "UTF-8").getLines.map { line =>
         line.split("""\b""").size
       }.toList

   val lineCount: File => List[Int] =
     (file: File) => Source.fromFile(file, "UTF-8").getLines.size :: Nil

  // 文件每一行的行数
  val lengths: File => List[Int] = f => {
    if (f.isDirectory) List(0)
    else Source.fromFile(f).getLines().toList map (l => l.length())
  }

  //val lineLengths = Kleisli(files) andThen lengths
  // 每一个文件的行数
  val composed = kleisli(files) >==> lineCount
  composed("/asa")

}

trait Scope {

  val files: String => List[File] =
    (dir) => new File(dir).listFiles().toList

  val lengths: File => List[Int] = (f) => {
    if (f.isDirectory) List(0)
    else Source.fromFile(f).getLines().toList map (l => l.length())
  }

  // Int => Option[String]
  def str(x: Int): Option[String] = Some(x.toString)

  // String => Option[Int]
  def toInt(x: String): Option[Int] = Some(x.toInt)

  // Int => Option[Double]
  def double(x: Int): Option[Double] = Some(x * 2)

  def oldSchool(i: Int) = for { x <- str(i) ; y <- toInt(x) ; z <- double(y) } yield z

  val funky: Kleisli[Option, Int, Double] = kleisli(str _) >==> (toInt _) >==> (double _)
  funky(1)

  val lineLengths = kleisli(files) >==> lengths

}

