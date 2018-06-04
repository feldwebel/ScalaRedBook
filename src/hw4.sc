class Rational(val n:Int, val d:Int) {
  val e = Rational.euclid(n, d)

  def *(k: Int): Rational = new Rational(n*k, d)
  def *(k: Rational): Rational = new Rational(n*k.n, d* k.d)

  def +(k: Int): Rational = new Rational(n*k*d, d*k)
  def +(k: Rational): Rational = new Rational(n*k.n, d*k.d)

  override def toString = {
    val i = n / d
    val n1 = (n - i * d) / e
    val d1 = d / e
    if (i != 0) s"$i $n1/$d1" else s"$n1/$d1"
  }
}

object Rational {
  def apply(n: Int, d: Int) = new Rational(n, d)

  @annotation.tailrec
  def euclid(a: Int, b: Int): Int =
    if (b == 0) a else euclid(b, a % b)
}

implicit class RationalOps(n: Int) {
  def ~/ (d: Int) = new Rational(n, d)
}


new Rational(2,5).*(5).+(new Rational(4,10))

2 ~/ 5 * 5 + 4 ~/ 10 // Rational...
// приоритет операторов, общий знаменатель, красивый toString

7 ~/ 8 * 10 + 8 ~/ 11