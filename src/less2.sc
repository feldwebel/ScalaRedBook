object less2 {

  trait PairLike[A, B] {
    def first: A
    def second: B
  }

  class Pair[A, B] (val first:A, val second:B) extends PairLike[A, B] {
    override def toString = s"($first, $second)"
    def swap:Pair[B, A] = new Pair(second, first)
    def reverseApply[C](f: (A, B) => C): C = f(first, second)
  }

  object Pair {
    def apply[A, B](first: A, second: B): Pair[A, B] = new Pair(first, second)
    // def apply[A, B](a: A, b: B) = new Pair(a, b)
  }

  implicit class PairOps[A](a:A) {
    def x[B](b:B):Pair[A,B] = Pair(a,b)

  }

  case class Rectangle(x1:Int, y1: Int, x2:Int, y2:Int)

  val t1 = 2 x 4 x 6 x 8

  val p1 = new Pair(0, 5)
  p1.first
  p1.second

  p1.swap.swap

  val p2 = Pair(7,8.8)

  val rectangle = Pair(p1, p2)

  val rect: Rect  = (0 x 6) x (3 x 8)

  type Point = Pair[Int, Int]

  type Rect = Pair[Point, Point]
/*  // identical to:
  val r2 = (
    new PairOps[Pair[Int,Int]](
      (new PairOps[Int](0).x(6))
    ).x(
      new PairOps[Int](3).x(8)
    )
    )

  def handleInput():Pair[Int, String] = ???
  def formatError(error: Pair[String, Int]): String = ???
  */

  def dist(p1:Point, p2:Point): Double = Math.sqrt(
    Math.pow(p1.first - p2.first, 2) + Math.pow(p1.second - p2.second, 2)
  )

  val r = (0 x 0) x (3 x 4)

  dist(r.first, r.second)

  r.reverseApply(dist)

  implicit class FuncOps[A, B, C](f: (A, B) => C) {
    def paired: Pair[A, B] => C = p => f(p.first, p.second)
    def paired2: Pair[A, B] => C = p => p reverseApply f
  }

  (dist _) paired r

  (dist _) paired2 r

  def trim(a: String): String = ???


}
