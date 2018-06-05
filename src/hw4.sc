class Rational(val n:Int, val d:Int) {
  case class Normalized(i: Int, n: Int, d: Int) {
    override def toString = {
      if (i != 0)
        s"$i $n/$d"
      else s"$n/$d"
    }
  }

  val e = Rational.euclid(n, d)
  val internal =
    Normalized(n / d, (n - (n / d) * d) / e, d / e)

  def normalizedN = internal.n
  def normalizedD = internal.d

  def *(k: Int): Rational = new Rational(n * k, d)
  def *(k: Rational): Rational =
    new Rational(n * k.n, d * k.d)

  def +(k: Int): Rational = new Rational(n * k * d, d * k)
  def +(k: Rational): Rational =
    new Rational(n * k.n, d * k.d)

  override def toString = internal.toString
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

val fract = 7 ~/ 8 * 10 + 8 ~/ 11

fract.normalizedN
fract.n
fract.d