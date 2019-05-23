object less180710 {
  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _ map f // x: Option[A] => x.map(f)

  lift(Math.sqrt) (Some(4.0))
  (Math.sqrt _).lifted(Some(4.0))

  implicit class funcLiftOps[A, B](f: A => B) {
    def lifted: Option[A] => Option[B] = lift(f)
    def callFromOption(oa: Option[A]): Option[B] = lift(f)(oa)
  }

  implicit class invoke[A](x: Option[A]) {
    def invokeLifted[B](f: A => B): Option[B] = lift(f)(x)
  }

  Some(4.0) invokeLifted Math.sqrt

  def sequence[A](xs: List[Option[A]]): Option[List[A]] =
    xs.foldRight(Some(List()):Option[List[A]])((i, a) => map2(a, i)(_ :+ _))

  def sequenceFC[A](xs: List[Option[A]]): Option[List[A]] =
    xs.foldRight(Some(List()):Option[List[A]])((i, a) =>
      for { aa <- a; ii <- i} yield aa :+ ii)

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f:(A, B) => C): Option[C] = {
    ao.flatMap(a => bo.map(b => f(a, b)))
  }

  def map3[A, B, C, D](ao: Option[A], bo: Option[B], co: Option[C])(f: (A, B, C) => D): Option[D] = {
    ao.flatMap(a => bo.flatMap(b => co.map(c => f(a, b, c))))
  }

  def map33[A, B, C, D](ao: Option[A], bo: Option[B], co: Option[C])(f: (A, B, C) => D): Option[D] =
    for {
      a <- ao
      b <- bo
      c <- co
    } yield f(a, b, c)

  object Logic {
    def calcInterest(thousands: Int, days: Int): Double = 0.00123 * days * thousands
  }

  def attempt[A](a: => A):Option[A] = {
    try {
      val aa = a
      Some(aa)
    } catch {case e: Exception => None}
  }

  def parseInts(xs: List[String]): Option[List[Int]] =
    sequence(xs map (s => attempt(s.toInt)))

  parseInts(List("11", "12", "13"))
  parseInts(List("11", "12", "WTF"))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(as map f)

  object CliApplication {
    def getArg(name:String):String = Map("t" -> "5", "d" -> "20").getOrElse(name, "")
    def go:Unit = {
      val thousands = parseInt(getArg("t"))
      val days      = parseInt(getArg("d"))
      val interest = map2(thousands, days) (Logic.calcInterest)
      val message =
         interest
        .map(i => s"Your interest is $i")
        .getOrElse("Fail to calc interest")
    }

    def parseInt(s:String):Option[Int] = attempt { s.toInt }
  }
}