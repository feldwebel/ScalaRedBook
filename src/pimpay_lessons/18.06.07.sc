

val l = List(1, 2, 3)
val s = List("a", "b", "c")

  l(0)
  l apply 1

  l.tail

  l.init

  l.map(_ * 2)

  l.max

  l.reduce(_ + _)
  l.reduce(_ * _)

  l.reduce(_ - _)

s.reduce(_ + _) reverse

def count[A](as:List[A]):Int = as.foldRight(0)((el, acc) => acc + 1)

count(s)
count(List())

type CustomerId = Int
type Platform = String
type Money = Double

def limits(c:CustomerId): Map[Platform, Money] = {
  Map(
    "sdek" -> 100.5,
    "beta" -> 50
  )
}

val vvv = limits(666).get("sdek")
limits(666).getOrElse("box", 0)

vvv

case class Point(x: Int, y: Int)
case class Color(rgb: String)

val picture = Map(
  Point(6, 8) -> Color("#aaaaaa"),
  Point(8, 0) -> Color("#000000")
)

def getPixel(p: Point): Color = picture.getOrElse(p, Color("#FFFFFF"))

getPixel(Point(6, 8))
getPixel(Point(0, 0))

l.groupBy(i => if ((i % 2) == 0) "even" else "odd" ) mapValues(_.size)
