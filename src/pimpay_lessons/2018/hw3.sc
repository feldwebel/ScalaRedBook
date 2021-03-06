sealed trait Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

def simplifyTop(e:Expr):Expr = e match {
  case BinOp(op, l, r) => simplify(BinOp(op, simplify(l), simplify(r)))
  case UnOp(op, a) => simplify(UnOp(op, simplify(a)))
  case _ => e
}

def simplify(e:Expr):Expr = e match {
  case BinOp("*", _, Number(0)) => Number(0) //first
  case BinOp("*", Number(0), _) => Number(0)
  case UnOp("-", UnOp("-", a)) => simplify(a)
  case UnOp("+", UnOp("+", a)) => simplify(a)
  case BinOp(_, a, Number(0)) => simplify(a) //first case already worked
  case BinOp("+", Number(0), a) => simplify(a)
  case BinOp("*", a, Number(1)) => simplify(a)
  case BinOp("*", Number(1), a) => simplify(a)
  case BinOp("+", Number(a), Number(b)) => Number(a + b)
  case BinOp("*", Number(a), Number(b)) => Number(a * b)
  case BinOp("+", a, UnOp("-", b)) => simplify(BinOp("-", a, b))
  case BinOp("-", a, UnOp("-", b)) => simplify(BinOp("+", a, b))
  case BinOp("*", a, UnOp("-", b)) => simplify(UnOp("-", BinOp("*", a, b)))
  case BinOp(op, l, r) => BinOp(op, simplify(l), simplify(r))
  case UnOp(op, a) => UnOp(op, simplify(a))
  case _ => e
}

// test

val ex1 =
  BinOp("+",
    BinOp("+",
      BinOp("*", Var("a"), Number(1)),
      UnOp("-", Number(0))
    ),
    BinOp("*",
      Var("b"),
      UnOp("-", Number(1))
    )
  )
simplifyTop(ex1)


simplifyTop(ex1) == BinOp("-", Var("a"), Var("b"))

var ex2 = BinOp("+", Number(14), Number(88))
simplify(ex2)
