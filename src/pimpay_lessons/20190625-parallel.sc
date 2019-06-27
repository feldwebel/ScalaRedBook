import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration.Duration

//trait Par[A]
type Par[A] = ExecutionContext => Future[A]

object Par {
  def unit[A](a: => A): Par[A] = _ => Future.successful(a)
  def combine[A, B, C](pa:Par[A], pb: Par[B])(f:(A, B) => C): Par[C] = ec => {
    val fa = pa(ec)
    val fb = pb(ec)

    implicit val context: ExecutionContext = ec
    for {
      a <- fa
      b <- fb
    } yield f(a, b)
  }
  def fork[A](pa:Par[A]): Par[A] = ec => Future(pa(ec))(ec).flatten
  def run[A](pa: Par[A])(ec: ExecutionContext): A = {
    val f = pa(ec)
    Await.result(f, Duration.Inf)
  }

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

def sleep(n:Int) = Par.lazyUnit({Thread.sleep(n * 1000); n})

val program1 = Par.combine(sleep(5), sleep(5))(_+_)
Par.run(program1)(scala.concurrent.ExecutionContext.global)