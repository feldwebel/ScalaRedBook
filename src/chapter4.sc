object MyChapter4 {

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def orElse[B >: A](a: => Option[B]): Option[B] = this match {
      case None => a
      case _ => this
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => this
      case _ => None
    }
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  val x = Some(88)
  val y = Some(14)
  val z = Seq(2.72, 3.14, 14.88, 66.6)
  def twice(a: Any): Option[Int] = a match {
    case a: Int => Some(a * 2)
    case _ => None
  }

  x.map(_ + 2)        //90
  x.getOrElse(33)     //88
  None.getOrElse("kuku")  // kuku
  x.flatMap(twice)    //Some(176)
  x.map(twice)        //Some(Some(176))
  None.flatMap(twice) // None
  None.orElse(x)      // Some(88)
  x.orElse(Some(99))  //
  x.filter(a => a > 80)
  y.filter(a => a > 80) // None

  variance(z)

}
