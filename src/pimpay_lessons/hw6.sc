sealed trait Expr {
  def height: Int
  def width: Int
}

case class Number(n:Double) extends Expr {
  val height = 1
  val width = n.toString.length
  override def toString = n.toString
}

case class Var(name:String) extends Expr {
  val height = 1
  val width = name.length
  override def toString = name
}
abstract class BinOp(val l:Expr, val r:Expr) extends Expr {
  val height = l.height max r.height
  val width = l.width + r.width + symbol.length + 2
  def symbol:String
  override def toString = s"$l $symbol $r"
}

case class Add(override val l:Expr, override val r:Expr) extends BinOp(l,r) {
  override def symbol: String = "+"
}

case class Sub(override val l:Expr, override val r:Expr) extends BinOp(l,r) {
  override def symbol: String = "-"
}

case class Mul(override val l:Expr, override val r:Expr) extends BinOp(l,r) {
  override def symbol: String = "*"
}

case class Div(override val l:Expr, override val r:Expr) extends BinOp(l,r) {
  override val width = (l.width max r.width) + 2
  override val height = l.height + r.height + 1
  override def symbol: String = "/"
}

val e = Div(
  Sub(
    Var("a"),
    Div(
      Add(Var("x"), Number(5)),
      Mul(Var("m"), Number(3.5))
    )
  ),
  Add(
    Div(Var("n"), Var("u")),
    Add(Var("sin(x)"), Div(Var("g"), Number(2)))
  )
)
e
e.height
e.width

val u = Var("sin(x)")
u.height
u.width

val v = Mul(Number(7), Add(Var("sin(x)"), Number(6)))

val dd = Add(Number(1), Div(Var("cos(x)"), Var("arccos(y)")))


case class Corner(x: Int, y: Int)

def plot(e:Expr): String = {
  val canvas = Array.fill(e.height, e.width)(' ')
  def populate(e: Expr, c: Corner): Unit = {
    def drawSimple(e: Expr, c: Corner) = {
      val up = c.y // + e.height / 2
      val out = e.toString

      for { i <- 1 until e.width} canvas(up)(c.x + i) = out(i-1)
    }
    def drawBin(e: BinOp, c: Corner) = {
      populate(e.l, Corner(c.x, (c.y + e.l.height)/2))
      canvas(c.y)(c.x + e.l.width + 1) = e.symbol(0)
      populate(e.r, Corner(c.x + e.l.width + 3, (c.y + e.r.height)/2))
    }
    def drawDiv(e: BinOp, c: Corner) = {
      populate(e.l, Corner(c.x + (e.width - e.l.width)/2, c.y))
      for (i <- 0 until e.width) canvas(c.y + e.l.height)(i) = '-'
      populate(e.r, Corner(c.x + (e.width - e.r.width)/2, c.y + e.l.height + 1))
    }
    e match {
      case a: Div => drawDiv(a, c)
      case a: Mul => drawBin(a, c)
      case a: Add => drawBin(a, c)
      case a: Sub => drawBin(a, c)
      case a: Expr => drawSimple(a, c)
    }
  }


  populate(e, Corner(0, 0))

  val result = StringBuilder.newBuilder
  for { i <- 0 until e.height} result.append(canvas(i).mkString + System.lineSeparator())

  result.toString()
}

println(plot(u))

println(plot(v))

println(plot(dd))

println(plot(e))



/**
  *
  * Homework:
def plot(e:Expr):String = ???
println(plot(e)) =>

         x + 5.0
    a - ---------
         m * 3.5
----------------------
  n               g
 --- + sin(x) + -----
  u              2.0
  */