
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}


  def sumSeq(ints:Seq[Int]):Int = ints.sum //ints.foldLeft(0)(_+_)

  //trait Par[A]

  type Par[A] = ExecutionContext => Future[A]

  object Par{
    def unit[A](a: A): Par[A] = _ => Future.successful(a)
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def map2[A,B,C](pa:Par[A], pb: Par[B])(f: (A,B) => C): Par[C] =
      ec => {
        val fa = pa(ec)
        val fb = pb(ec)

        fa.flatMap(a => fb.map(b => f(a,b))(ec))(ec)
    }

    def fork[A](pa: => Par[A]): Par[A] = ec => Future(pa(ec))(ec).flatten //???

    def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
      map(parList)(_.sorted)

    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a,_) => f(a))

    def sequence[A](l:List[Par[A]]): Par[List[A]] =
      l.foldRight(unit(List.empty[A]))((i, a) => map2(a, i)(_ :+ _))

    def parMap[A, B](la: List[A])(f: A => B): Par[List[B]] = fork {
      val a = la.map(asyncF(f))
      sequence(a)
    }

    def choose[A](cond: Par[Boolean])(t: => Par[A], f: => Par[A]):Par[A] = ec => {
      if (run(cond)(ec)) t(ec) else f(ec)
    }

    def chooseN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] = ec =>
      choices(run(pn)(ec))(ec)

    def choose2[A](cond: Par[Boolean])(t: => Par[A], f: => Par[A]):Par[A] = {
      chooseN(map(cond)(a => if (a) 1 else 0))(List(f, t))
    }

    def chooseMap[K,V](pk:Par[K])(map: Map[K, Par[V]]): Par[V] =
      ec => map(run(pk)(ec))(ec)

    def choose3[A](cond: Par[Boolean])(t: => Par[A], f: => Par[A]):Par[A] =
      chooseMap(map(cond)(a => if (a) 1 else 0))(Map(0 -> f, 1 -> t))

    def chooseN2[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
      chooseMap(pn)(choices.zipWithIndex.map(_.swap).toMap)

    def chooser[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = //flatMap
      ec => f(run(pa)(ec))(ec)

    def chooseMap2[K,V](pk:Par[K])(map: Map[K, Par[V]]): Par[V] =
      chooser(pk)(a => map(a))

    def chooseN3[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
      chooser(pn)(a => choices(a))

    def choose4[A](cond: Par[Boolean])(t: => Par[A], f: => Par[A]):Par[A] =
      chooser(cond)(c => if (c) t else f)

/*    Дз
    1) через unit + flatMap (не меняем fork, run)
    выразить map, map2
    и
    def join[A](ppa:Par[Par[A]]):Par[A] = ???
    через unit и flatMap*/


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
      Par.map2(sumL, sumR)(_ + _)
      //Par.get(sumL) + Par.get(sumR)
      //sum(a) + sum(b)
    }
  }

val ec = scala.concurrent.ExecutionContext.global;
  var prog1 = sum(Vector(1, 2, 3, 4))
  Par.run(prog1)(scala.concurrent.ExecutionContext.global)

  val x = Par.lazyUnit(List(2, 12, 85, 6))
  Par.run(Par.sortPar(x))(scala.concurrent.ExecutionContext.global)

def pingServer(name: String): Int = {
  val t = name.toInt * 1000
  Thread.sleep(t)
  t
}

val servers = (for{i <- 1 to 5} yield i.toString).toList
servers

val prog2 = Par.map(Par.parMap(servers)(pingServer))(l => l.sum.toDouble / l.length)
Par.run(prog2)(ec)

val fixed = java.util.concurrent.Executors.newFixedThreadPool(2)
Par.run(prog2)(scala.concurrent.ExecutionContext.fromExecutorService(fixed))


