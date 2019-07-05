import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.higherKinds

import zio._


// generic algebra
trait ParEngine[Par[_], Ctx] {
  self =>


  def unit[A](a: A):Par[A]
  def fork[A](pa: => Par[A]):Par[A]
  def combine[A,B,C](pa:Par[A], pb: Par[B])(f: (A,B) => C):Par[C]
  def run[A](pa:Par[A])(ec:Ctx):A
  def flatMap[A,B](pa:Par[A])(f: A => Par[B]):Par[B]

  // derivatives
  def lazyUnit[A](a: => A):Par[A] = fork(unit(a))
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
  def map[A,B](pa:Par[A])(f:A => B):Par[B] = combine(pa, unit(()) )( (a,_) => f(a) )
  def parSort(l:Par[List[Int]]):Par[List[Int]] = map(l)(_.sorted)
  def parMap[A,B](l:List[A])(f: A => B):Par[List[B]] = fork { traverse(l)(asyncF(f)) }
  def traverse[A,B](l:List[A])(f: A => Par[B]):Par[List[B]] = l.foldLeft(unit(List.empty[B]))( (acc, el) => {
    combine(f(el), acc)(_ :: _)
  })
  def sequence[A](l:List[Par[A]]):Par[List[A]] = traverse(l)(identity)

  // implicit class
  implicit class ParOps[A](pa:Par[A]) {
    def map[B](f: A => B):Par[B] = self.map(pa)(f)
    def flatMap[B](f: A => Par[B]):Par[B] = self.flatMap(pa)(f)
  }
}

// future engine
type Par[A] = ExecutionContext => Future[A]
object FutureEngine extends ParEngine[Par, ExecutionContext] {
  def unit[A](a: A):Par[A] = _ => Future.successful(a)
  def fork[A](pa: => Par[A]):Par[A] = implicit ec => Future(pa(ec)).flatten
  override def flatMap[A, B](pa: Par[A])(f: (A) => Par[B]): Par[B] = ec => {
    pa(ec).flatMap(a => f(a)(ec))(ec)
  }
  def combine[A,B,C](pa:Par[A], pb: Par[B])(f: (A,B) => C):Par[C] = implicit ec =>
    pa(ec) zip pb(ec) map f.tupled
  def run[A](pa:Par[A])(ec:ExecutionContext):A = Await.result(pa(ec), Duration.Inf)
}

// mock engine
type Mock[A] = A
object MockEngine extends ParEngine[Mock, Unit] {
  override def unit[A](a: A): Mock[A] = a
  override def fork[A](pa: => Mock[A]): Mock[A] = pa
  override def combine[A, B, C](pa: Mock[A], pb: Mock[B])(f: (A, B) => C): Mock[C] = f(pa,pb)
  override def flatMap[A, B](pa: Mock[A])(f: (A) => Mock[B]): Mock[B] = f(pa)
  override def run[A](pa: Mock[A])(ec: Unit): A = pa
}

// ZIO
type UIOFiber[A] = UIO[Fiber[Nothing,A]] // ZIO[Any,Nothing,Fiber[Nothing,A]]


trait ZioEngine[R] extends ParEngine[UIOFiber, Runtime[R]]  {
  override def unit[A](a: A): UIOFiber[A] = UIO.succeed(Fiber.succeed(a))
  override def fork[A](pa: => UIOFiber[A]): UIOFiber[A] = UIO.unit.fork.flatMap(_ => pa)
  override def combine[A, B, C](pa: UIOFiber[A], pb: UIOFiber[B])(f: (A, B) => C): UIOFiber[C] = {
    pa.zipWithPar(pb) { _.zipWith(_)(f) }
  }

  override def flatMap[A, B](pa: UIOFiber[A])(f: (A) => UIOFiber[B]): UIOFiber[B] = {
    pa.flatMap(_.join).flatMap(f)
  }

  override def run[A](pa: UIOFiber[A])(ec: Runtime[R]): A = {
    ec.unsafeRun(pa.flatMap(_.join))
  }
}
/**
type PureFiber[A] = Fiber[Nothing,A]
trait PureFiberEngine[R] extends ParEngine[PureFiber, Runtime[R]]  {
  override def unit[A](a: A): PureFiber[A] = Fiber.succeed(a)
  override def fork[A](pa: => PureFiber[A]): PureFiber[A] = Fiber.succeedLazy(pa)
  override def combine[A, B, C](pa: PureFiber[A], pb: PureFiber[B])(f: (A, B) => C): PureFiber[C] = {
    pa.zipWith(pb)(f)
  }
  override def run[A](pa: PureFiber[A])(ec: Runtime[R]): A = {
    ec.unsafeRun(pa.join)
  }
} TODO ???*/

def sleep(n:Int):Int = {
  Thread.sleep(n * 1000)
  n
}

object App {
  def logic[Par[_], Ctx](PE:ParEngine[Par, Ctx])(ctx:Ctx):Int = {

    import PE._

    val program = for {
      sleeps <- parMap(List(1,2,3))(sleep)
      r <- lazyUnit(sleep(sleeps.min))
    } yield r

    PE.run(program)(ctx)
  }
}


def benchmark[A](label:String, f: => A):String = {
  val start = System.currentTimeMillis()
  val r = f
  val end = System.currentTimeMillis()

  val duration = end - start

  s"Program $label returned $r and took $duration ms"
}



// end of universe
benchmark("Future", App.logic(FutureEngine)(scala.concurrent.ExecutionContext.global))
benchmark("Mock", App.logic(MockEngine) () )
benchmark("ZIOFiber", App.logic(new ZioEngine[Any] {})(new DefaultRuntime {}))