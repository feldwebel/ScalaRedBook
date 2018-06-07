sealed trait Expr

case class Number(n:Double) extends Expr {
  override def toString = n.toString
}

case class Var(name:String) extends Expr {
  override def toString = name
}
abstract class BinOp(val l:Expr, val r:Expr) extends Expr {
  def symbol:String
  override def toString = s"($l) $symbol ($r)"
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