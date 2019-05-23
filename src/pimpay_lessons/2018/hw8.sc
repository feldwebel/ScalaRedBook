// 1. traverse через один foldRight
// 2. sequence через traverse


object MyEither {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B):Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
    def flatMap[EE >: E, B](f: A => Either[EE,B]):Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
    def orElse[EE >: E, AA >: A](other: => Either[EE, AA]):Either[EE, AA] = this match {
      case Right(_) => this
      case Left(_) => other
    }
    def filter[EE >: E, AA >: A](p: AA => Boolean, e: => EE): Either[EE, AA] =
      map((i: A) => if (p(i)) this else Left(e)).orElse(e)

    def mapError[E2](f: E => E2): Either[E2, A] = this match {
      case Right(a) => Right(a)
      case Left(e) => Left(f(e))
    }

    def recover[AA >: A](f: PartialFunction[E, AA]): Either[E, AA] = this match {
      case Left(e) => if(f.isDefinedAt(e)) Right(f(e)) else Left(e)
      case Right(e) => Right(e)
    }

    def toOption:Option[A] = this match {
      case Right(a) => Some(a)
      case _ => None
    }
  }

  case class Left[+E](e: E) extends Either[E, Nothing]
  case class Right[+A](v: A) extends Either[Nothing, A]


  def attempt[A](a: => A):Either[Exception, A] = {
    try {
      val aa = a
      Right(a)
    } catch {case e: Exception => Left(e)}
  }

  val a = Some(15)
  val b = None:Option[Int]

  a.toEither("error")

  def sequence[E,A](xs:List[Either[E,A]]):Either[E, List[A]] = traverse(xs)(identity)
  def traverse[E,A,B](xs:List[A])(f: A => Either[E,B]):Either[E, List[B]] =
    xs.foldRight[Either[E, List[B]]](Right(List()))((i:A, a:Either[E,List[B]]) =>
      for {ii <- f(i); aa <- a} yield aa :+ ii)

  "bad argument".asLeft[Int]
  123.asRight[Exception]
  Right(123).asRight[Exception]

  implicit class EitherOps[A](a:A) {
    def asLeft[B]:Either[A,B] = Left(a)
    def asRight[E]:Either[E,A] = Right(a)
  }

  implicit class EitherOps1[A](a:Right[A]) {
    def asRight[E]:Either[E,A] = a
  }

  implicit class OptionOps[A](a: Option[A]) {
    def toEither[E](s: => E):Either[E, A] = a.map(_.asRight[E]).getOrElse(s.asLeft[A])
  }
}

