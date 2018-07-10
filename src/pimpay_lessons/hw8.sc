// 1. traverse через один foldRight
// 2. sequence через traverse


object MyEither {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B):Either[E, B] = ???
    def flatMap[EE >: E, B](f: A => Either[EE,B]):Either[E,B] = ???
    def orElse[EE >: E, AA >: A](other: => Either[EE, AA]):Either[EE, AA] = ???
    def filter[EE >: E, AA >: A](p: AA => Boolean, e: => EE): Either[EE, AA] = ???
  }

  case class Left[+E](e: E) extends Either[E, Nothing]
  case class Right[+A](v: A) extends Either[Nothing, A]


  def attempt[A](a: => A):Either[Exception, A] = {
    try {
      val aa = a
      Right(a)
    } catch {case e: Exception => Left(e)}
  }

  def sequence[E,A](xs:List[Either[E,A]]):Either[E, List[A]] = ???
  def traverse[E,A,B](xs:List[A])(f: A => Either[E,B]):Either[E, List[B]] = ???

}

