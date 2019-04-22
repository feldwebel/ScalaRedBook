import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

object Solution {

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val n = stdin.readLine.trim.toInt
    var i = 1
    var m = Array.empty[(String, String)]
    var k = Array.empty[String]

    for (i <- 1 to n){
      val a = stdin.readLine().split(" ")
      m :+ a
    }
    for (i <- 1 to n){
      val a = stdin.readLine()
      k :+ a
    }


    //val arr = stdin.readLine.split(" ").map(_.trim.toInt)

    println(m.toString)
    println(k.toString)
  }
}