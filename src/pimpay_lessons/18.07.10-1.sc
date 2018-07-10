case class Option[A]() {
  def flatMap[B](f: A => Option[B]):Option[B] = Option[B]()
  def map[B](f: A => B):Option[B] = Option[B]()
}


for {
  a <- Option[Int]()
  b <- Option[String]()
} yield 1 + 2
