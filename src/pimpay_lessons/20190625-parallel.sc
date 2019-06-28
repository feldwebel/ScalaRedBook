import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration.Duration

//trait Par[A]
type Par[A] = ExecutionContext => Future[A]

object Par {
  def unit[A](a: => A): Par[A] = ec => Future.successful(a)
  def combine[A, B, C](pa:Par[A], pb: Par[B])(f:(A, B) => C): Par[C] = implicit ec =>
    pa(ec) zip pb(ec) map f.tupled

  def combine1[A, B, C](pa:Par[A], pb: Par[B])(f:(A, B) => C): Par[C] = ec => {
    val fa = pa(ec)
    val fb = pb(ec)
    //(fa zip fb).flatten
    implicit val context: ExecutionContext = ec
    for {
      a <- fa
      b <- fb
    } yield f(a, b)
  }
  def delay[A](pa:Par[A]): Par[A] = ec => pa(ec)
  def fork[A](pa:Par[A]): Par[A] = ec => Future(pa(ec))(ec).flatten
  def run[A](pa: Par[A])(ec: ExecutionContext): A = {
    val f = pa(ec)
    Await.result(f, Duration.Inf)
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parSort(l:Par[List[Int]]): Par[List[Int]] = combine(l, unit())((a, _) => a.sorted)

  def map[A, B](pa:Par[A])(f:A => B): Par[B] = ???   //no ec

  def parMap[A, B](l:List[A])(f: A => B): Par[List[B]] = ???
}

def sum(s:Seq[Int]):Par[Int] = {
  if (s.length <= 1) Par.unit(s.headOption getOrElse 0) else {
    val (l, r) = s.splitAt(s.length/2)

    Par.combine(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }
}

val program0 = Par.lazyUnit((for { i <- 1 to 5 } yield scala.util.Random.nextInt(i)).toList)

val program = Par.parSort(program0)
/*
val program = sum(Vector(1, 2, 3, 4) )
Par.run(program)(scala.concurrent.ExecutionContext.global)

def sleep(n:Int) = Par.lazyUnit({Thread.sleep(n * 1000); n})

val program1 = Par.combine(sleep(5), sleep(5))(_+_)
Par.run(program1)(scala.concurrent.ExecutionContext.global)*/
