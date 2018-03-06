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

  x.toList
  x.take(3).toList
  x.drop(2).toList
  x.takeWhile(_ < 3).toList
  x.forAll(_.isValidInt)
  x.takeWhileFoldRight(_ < 3).toList



}