
object MyChapter5 {

  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case _            => Nil
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 0 => Stream.cons(hd(), tl().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(hd, tl) if p(hd()) => Stream.cons(hd(), tl().takeWhile(p))
      case _ => Empty
    }

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhileFoldRight(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((hd, tl) => if (p(hd)) Stream.cons(hd, tl) else Empty)

    def headOptionFoldRight: Option[A] =
        foldRight(None: Option[A])((hd, _) => Some(hd))

    def map[B](f: A => B): Stream[B] =
      foldRight(Empty: Stream[B])((hd, tl) => Stream.cons(f(hd), tl))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((hd, tl) => if (p(hd)) Stream.cons(hd, tl) else tl)

    def append[B>:A](s: Stream[B]): Stream[B] =
      foldRight(s)((hd, tl) => Stream.cons(hd, tl))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Empty: Stream[B])((h,t) => f(h) append t)
  }

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def f1(a: Int, b: Int): Stream[Int] = Stream.cons(a, f1(b, a + b))
    f1(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  }

  val x = Stream(1, 2, 3, 4, 5, 6);
  val y = Stream(7, 8, 9, 10)
  val z = Stream(Stream(1))

  def twice(a: Any): Option[(Int, Int)] = a match {
    case a: Int => Some((a, a * 2))
    case _ => None
  }

  x.toList
  x.take(3).toList
  x.drop(2).toList
  x.takeWhile(_ < 3).toList
  x.forAll(_.isValidInt)
  x.takeWhileFoldRight(_ < 3).toList
  x.headOption
  x.headOptionFoldRight
  x.map(_ * 2).toList
  x.filter(_ % 2 == 0).toList
  x.append(y).toList
  x.flatMap(Stream(_)).toList
  constant(x).take(5).toList
  from(9).take(10).toList
  val f = fibs
  unfold(1)(twice).take(5).toList

  def fibonacci(ab: (Int, Int)): Option[(Int, (Int, Int))] = ab match {
    case (a, b) => Some((a, (b, a + b)))
    case _ => None
  }
  unfold((0, 1))(fibonacci).take(10).toList

  def from1(n: Int): Option[(Int, Int)] = Some((n, n+1))
  unfold(9)(from1).take(10).toList

  def ones1: Option[(Int, Unit)] = Some((1,()))
  unfold(())(_ => ones1).take(10).toList

  def constant1[A](a: A): Option[(A, A)] = Some((a, a))
  unfold(88)(constant1).take(5).toList


}