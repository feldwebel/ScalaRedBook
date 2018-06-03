class Rational(val n:Int, val d:Int) {
  override def toString = s"Rational($n, $d)"
  def *(k: Int): Rational = new Rational(n*k, d)
  def *(k: Rational): Rational = new Rational(n*k.n, d* k.d)

  def +(k: Int): Rational = new Rational(n*k*d, d*k)
  def +(k: Rational): Rational = new Rational(n*k.n, d*k.d)
}

object Rational {
  def apply(n: Int, d: Int) = new Rational(n, d)

}

implicit class RationalOps(n: Int) {
  def @@ (d: Int) = new Rational(n, d)
}


new Rational(2,5).*(5).+(new Rational(4,10))

2 @@ 5 * 5 + 4 @@ 10 // Rational...
// приоритет операторов, общий знаменатель, красивый toString
