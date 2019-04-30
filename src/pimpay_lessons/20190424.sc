// Part 1
def t[A](a: => A): A = {
  println("Calcing..." + a); a
}

val x = true  ? t(1) <=> t(2)
val y = false ? Option(1) << None

implicit class TernaryOps(cond: Boolean) {
  def ?[A](onTrue: =>A) = new TernaryWrapper[A](cond, onTrue)
}

class TernaryWrapper[A](cond: Boolean, onTrue: =>A) {
  def <=>(onFalse: =>A): A = if (cond) onTrue else onFalse
  def <<(onFalse: =>A): A = if (cond) onTrue else onFalse
}

// Part 2
trait Table
trait Column
trait ColumnExpr[+A]

type BIGINT = Int
type TEXT = String

object tbl_user extends Table {
  def id = new ColumnExpr[BIGINT] {}
  def name = new ColumnExpr[TEXT] {}
  def customer_id = new ColumnExpr[BIGINT] {}
}

object tbl_customer extends Table {
  def id = new ColumnExpr[BIGINT] {}
  def name = new ColumnExpr[TEXT] {}
}

object SELECT {
  def apply(args:ColumnExpr[Any]*) = new SqlPreFromExpr {}

}

trait BaseSqlExpr

trait SqlPreFromExpr extends BaseSqlExpr {
  def FROM(t:Table) = new JoinableExpr {}
}

trait JoinableExpr extends BaseSqlExpr {
  def JOIN(t:Table) = new JoinableExpr {}
}

object * extends ColumnExpr[Any] {}

val sql = (
  SELECT (tbl_user.id, tbl_user.name) FROM tbl_user
    JOIN tbl_customer
  )


// Part 3
case class ThePair[A,B](a: A, b: B)

// How it compiles
class ThePair2[A,B](val a: A, val b: B) {
  override def toString = s"ThePair2($a,$b)"
  override def equals(obj: scala.Any): Boolean = if (obj.isInstanceOf[ThePair2[A,B]]) {
    val p2 = obj.asInstanceOf[ThePair2[A,B]]
    a == p2.a && b == p2.b
  } else false
}

val m = Map("sdek" -> 1000d, "beta" -> 50d)

object ThePair2 {
  def apply[A,B](a: A, b: B) = new ThePair2(a,b)
  def unapply[A,B](p:ThePair2[A,B]):Option[(A,B)] = Option(p.a -> p.b)
}


val p1 = ThePair(1, 2)
val p11 = ThePair(1, 2)
p1.a
p1

val p2 = ThePair2(1, 2)
val p22 = ThePair2(1, 2)
p2.a
p2

p1 match {
  case ThePair(_, b) => b
}

p2 match {
  case ThePair2(_, b) => b
}

p1 == p11
p2 == p22