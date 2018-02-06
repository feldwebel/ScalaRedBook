object MyChapter4 {

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

        //def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }
    //    def orElse[B >: A](ob: => Option[B]): Option[B]
    //    def filter(f: A => Boolean): Option[A]

  }

  val a:Option[Int] = Some(85)

  a.map(_ + 2)
  a.getOrElse(33)
  None.getOrElse("kuku")



}
