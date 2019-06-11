// extractors
val x = 5

class Point(val x:Int, val y:Int)

object MyPoint {
  def apply(x:Int, y:Int):Point = new Point(x,y)
  def unapply(p:Point):Option[(Int,Int)] = Option(p.x, p.y)
}

object Positive {
  def unapply(i:Int):Boolean = i > 0

}

object Sqr {
  def unapply(i:Int):Option[Int] = Option(i * i)
}


val p1 = MyPoint(2, 5)


object First {
  def unapply[A](a:A):Option[A] = Option(a)
  def unapply[A,B](t2:(A,B)):Option[A] = Option(t2._1)
  def unapply[A,B,C](t3:(A,B,C)):Option[A] = Option(t3._1)
}

object ToInt {
  def unapply(s:String):Option[Int] =
  def unapply(i:Int):Option
}

val r = (2,4,5) match {
  case ToInt(f) => f
}


val ff = for {
  Sqr(x) <- Some(5)
} yield x

val ToInt(Positive(i)) = s


r

