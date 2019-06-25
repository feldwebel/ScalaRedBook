import scala.concurrent.{Future, ExecutionContext}

//trait Par[A]
type Par[A] = ExecutionContext => Future[A]




object Par {
  def unit[A](a: => A): Par[A] = ???
  def combine[A, B, C](pa:Par[A], pb: Par[B])(f:(A, B) => C): Par[C] = ???
  def fork[A](pa:Par[A]): Par[A] = ???

  def run[A](pa: Par[A])(ec: ExecutionContext): A = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
}

def sum(s:Seq[Int]):Par[Int] = {
  if (s.length <= 1) Par.unit(s.headOption getOrElse 0) else {
    val (l, r) = s.splitAt(s.length/2)

    Par.combine(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }
}

val program = sum(Vector(1, 2, 3, 4) )
Par.run(program)(scala.concurrent.ExecutionContext.global)