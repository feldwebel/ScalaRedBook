object less04 {

class Rational1(val n:Int, val d:Int) {
  override def toString = s"Rational1($n, $d)"
  def copy(n: Int = n, d: Int = d):Rational1 = Rational1(n, d)
  def *(k: Int): Rational1 = copy(n = n * k)

  override def equals(obj: scala.Any): Boolean = obj match {
    case Rational1(n2, d2) => n == n2 && d == d2
    //case o:Rational1 => n == o.n && d == o.d
    case _ => false
  }
}

object Rational1 {
  def apply(n: Int, d:Int) = new Rational1(n, d)
  def unapply(r:Rational1):Option[(Int, Int)] = Some((r.n, r.d))
}

  val r1 = new Rational1(3, 4)
  val r12 = new Rational1(3, 4)
  val r11 = Rational1(4, 5)
  val r111 = r11 * 2

  r1 == r12 //false


case class Rational2(n:Int, d:Int) {
  def *(k:Int): Rational2 = copy(n = n*k)//Rational2(n*k, d)
}

  val r2 = Rational2(5, 8)
  val r22 = Rational2(5, 8)

  r2 == r22

val `r2 * 2` = new Rational2(r2.n * 2, r2.d)

  val r222 = r2 * 2


}