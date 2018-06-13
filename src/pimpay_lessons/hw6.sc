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

val u = Var("n")

case class Corner(x: Int, y: Int)

def plot(e:Expr): String = {
  val matrix = Array.ofDim[Char](e.height - 1, e.width - 1)
  def populate(e: Expr, c: Corner) = {
    def drawSimple(e: Expr, c: Corner) = {
      val up = c.y + e.height / 2
      val l = c.x
      val out = e.toString
      for { i <- 0 to e.width} matrix(up)(c.x + i) = out(i)
    }
    def drawBin(e: BinOp, c: Corner) = {}
    def drawDiv(e: BinOp, c: Corner) = {}
    e match {
      case a: Div => drawDiv(a, c)
      case a: Mul => drawBin(a, c)
      case a: Add => drawBin(a, c)
      case a: Sub => drawBin(a, c)
      case a: Var => drawSimple(a, c)
      case a: Number => drawSimple(a,c)
    }
  }


  populate(e, Corner(0, 0))

  for { i <- 0 until e.height-1} println(matrix(i).mkString)
}

plot(u)
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