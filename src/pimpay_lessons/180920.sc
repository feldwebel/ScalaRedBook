
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}


  def sumSeq(ints:Seq[Int]):Int = ints.sum //ints.foldLeft(0)(_+_)

  //trait Par[A]

  type Par[A] = ExecutionContext => Future[A]

  object Par{
    def unit[A](a: A): Par[A] = _ => Future.successful(a)
    def forkUnit[A](a: => A): Par[A] = fork(unit(a))
    def combine[A,B,C](pa:Par[A], pb: Par[B])(f: (A,B) => C): Par[C] =
      ec => {
        val fa = pa(ec)
        val fb = pb(ec)

        fa.flatMap(a => fb.map(b => f(a,b))(ec))(ec)
    }

    def fork[A](pa: => Par[A]): Par[A] = ec => Future.apply(pa(ec))(ec).flatten //???

    def run[A](pa: Par[A])(ec: ExecutionContext): A = {
      val f = pa(ec)
      Await.result(f, Duration.Inf)
    }
  }

  def sum(ints:IndexedSeq[Int]):Par[Int] = {
    if (ints.size <= 1) Par.unit(ints.headOption getOrElse 0)
    else {
      val (a, b) = ints.splitAt(ints.size / 2)
      val sumL = Par.fork(sum(a)) //Par.unit(sum(a))
      val sumR = Par.fork(sum(b)) //Par.unit(sum(b))
      Par.combine(sumL, sumR)(_ + _)
      //Par.get(sumL) + Par.get(sumR)
      //sum(a) + sum(b)
    }
  }

  var prog1 = sum(Vector(1, 2, 3, 4))
  Par.run(prog1)(scala.concurrent.ExecutionContext.global)

