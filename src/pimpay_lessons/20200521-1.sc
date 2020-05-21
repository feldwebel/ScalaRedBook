import shapeless._


case class Point(x: Int, y: Int)
case class Pair(p1:Int, p2:Int)

val p = Point(2,10)

val gen = Generic[Point].to(p)

val t2 = Generic[(Int,Int)].from(gen)


val t3 = Generic[(Int,Int)].from(10 :: 5 :: HNil)