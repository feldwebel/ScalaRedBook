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

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (a1 => b map (b1 => f(a1, b1)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (h1 => sequence(t) map (h1 :: _))
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => for {
        hv <- f(h)
        r <- traverse(t)(f)
      } yield hv :: r
    }

  val x = Some(88)
  val y = Some(14)
  val z = Seq(2.72, 3.14, 14.88, 66.6)
  val q = List(2, 3, 14, 66)
  val w = List(Some(2.72), Some(3.14), Some(14.88), Some(66.6))
  val v = List("[eq", "govno", "muravei")
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

  map2(x, y)((a, b) => a * b)

  sequence(w)

  traverse(v)(twice)
  traverse(q)(twice)

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]
  sealed trait Either[+E, +A]{
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => b
      case Right(a) => this
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap(a1 => b.map(b1 => f(a1,b1)))

  }

}
