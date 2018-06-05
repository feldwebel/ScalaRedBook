class Rational(nn:Int, dd:Int) {
  case class Normalized(i: Int, n: Int, d: Int) {
    override def toString =
      if (i != 0) s"$i $n/$d" else s"$n/$d"
  }

  val e = Rational.euclid(nn, dd)
  val n = nn / e
  val d = dd / e
  val internal = Normalized(n / d, (n - (n / d) * d) / e, d / e)

  def normalizedN = internal.n
  def normalizedD = internal.d

  def *(k: Rational): Rational = new Rational(n * k.n, d * k.d)
  //def *(k: Int): Rational = *(Rational(k, 1))

  //def +(k: Int): Rational = new Rational(n + k * d, d * k)
  def +(k: Rational): Rational = new Rational(n * k.d + k.n * d, d * k.d)
  //def +(k: Int): Rational = this + Rational(k, 1)

  override def toString = s"$n/$d"
  def normalized = internal.toString
}

object Rational {
  def apply(n: Int, d: Int) = new Rational(n, d)

  @annotation.tailrec
  def euclid(a: Int, b: Int): Int =
    if (b == 0) a else euclid(b, a % b)
  object Implicits {
    implicit class RationalOps(n: Int) {
      def ~/ (d: Int) = new Rational(n, d)
    }

    implicit def IntToRational(i: Int): Rational = Rational(i, 1)
  }
}

{
  import Rational.Implicits._

  new Rational(2,5).*(5).+(new Rational(4,10)) // 12/5

  2 ~/ 5 * 5 + 4 ~/ 10 // 12/5

  val fract: Rational = 7 ~/ 8 * 10 + 8 ~/ 11  // 417/44

  fract.normalizedN // 21
  //fract.n           // 417
  //fract.d           // 44
  //fract             // 417/44
  fract.normalized  // 9 21/44
  println(fract)
}


