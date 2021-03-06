import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

type Par[A] = ExecutionContext => Future[A]

object Par{
  def unit[A](a: A): Par[A] = _ => Future.successful(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = //flatMap
    ec => f(run(pa)(ec))(ec)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    flatMap(pa)(a => unit(f(a)))

  def map2[A,B,C](pa:Par[A], pb: Par[B])(f: (A,B) => C): Par[C] =
    flatMap(pa)(a => map(pb)(b => f(a, b)))

  def fork[A](pa: => Par[A]): Par[A] = ec => Future(pa(ec))(ec).flatten //???

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def sequence[A](l:List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(List.empty[A]))((i, a) => map2(a, i)(_ :+ _))

  def parMap[A, B](la: List[A])(f: A => B): Par[List[B]] = fork {
    val a = la.map(asyncF(f))
    sequence(a)
  }

  def chooseMap[K,V](pk:Par[K])(map: Map[K, Par[V]]): Par[V] =
    flatMap(pk)(a => map(a))

  def chooseN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(pn)(a => choices(a))

  def choose[A](cond: Par[Boolean])(t: => Par[A], f: => Par[A]):Par[A] =
    flatMap(cond)(c => if (c) t else f)

  def join[A](ppa: Par[Par[A]]): Par[A] =
    flatMap(ppa)(a => a)

  def run[A](pa: Par[A])(ec: ExecutionContext): A = {
    val f = pa(ec)
    Await.result(f, Duration.Inf)
  }
}


case class Position(x: Int, y: Int) {
  def isOk(p: Position): Boolean =
    p.x != x || p.y != y || (p.x - x).abs != (p.y - y).abs
}

type Board = List[Position]



/*case class Board() {
  var board = ArrayBuffer.empty[Position]
  def isCorrect(p1: Position): Boolean =
    board.forall(_.isOk(p1))

  def add(p: Position) =
    if (isCorrect(p)) board+=p else board
}*/

def setQueens(n: Int): Board = {
  def placeQueens

  if (n > 3) {

  } else {
    List.empty[Position]
  }
}


def queens(n: Int): List[Board] = {
  def placeQueens(k: Int): List[Board] =
    if (k == 0)
      List(List())
    else
      for {
        board <- placeQueens(k - 1)
        column <- 1 to n
        queen = Position(k, column)
        if queen.isOk(board)
      } yield queen :: board

  placeQueens(n)
}

queens(5)
