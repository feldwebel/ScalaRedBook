// Lazy List -- Stream

sealed trait Stream[+A]

case class SCons[A](h: () => A, t: () => Stream[A]) extends Stream[A]
case object SNil extends Stream[Nothing]

object Stream{
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val hh = h
    lazy val tt = t
    SCons ( () => hh, () => tt)
  }
  def nil = SNil
  def empty[A]: Stream[A] = SNil
  def apply[A](a: A*): Stream[A] = if (a.isEmpty) empty[A] else cons(a.head, apply(a.tail:_*))

  def toList[A](s: Stream[A]): List[A] = s match {
    case SCons(h,t) => h() +: toList(t())
    case SNil => Nil
  }

  /*def headOption[A](s:Stream[A]): Option[A] = s match {
    case SCons(h, _) => Option(h())
    case _ => None
  }

  def take[A](s: Stream[A], n: Int): Stream[A] = s match {
    case SCons(h, t) if n > 1 => cons(h(), take(t(), n - 1))
    case SCons(h, _) if n > 0 => cons(h(), take(t(), nil))
    case _ => SNil
  }

  def exitst[A](s: Stream[A])(p: A => Boolean): Boolean = foldRight(s, false)((el, acc) => p(el) || acc)
  */
  def foldRight[A, B](s: Stream[A], z:B )(f: (A, =>B) => B): B = s match {
    case SCons(h,t) => f(h(), foldRight(t(), z)(f))
    case SNil => z
  }


  def headOption[A](s:Stream[A]): Option[A] =
    foldRight(s, Option.empty[A])((el, _) => Option(el))

  def take[A](s:Stream[A], n:Int):Stream[A] =
    foldRight(s, Stream.empty[A])((el, acc) => if (n == 0) nil else cons(el, take(acc, n-1)))

  def exists[A](s:Stream[A])(p: A => Boolean):Boolean =
    foldRight(s, false)((el,acc) => p(el) || acc)
  def forall[A](s:Stream[A])(p: A => Boolean):Boolean =
    foldRight(s, true)((el, acc) => p(el) && acc )

  def filter[A](s:Stream[A])(p: A => Boolean): Stream[A] =
    foldRight(s, Stream.empty[A])((el, acc) => if (p(el)) cons(el, acc) else acc)
  def map[A,B](s:Stream[A])(f: A => B):Stream[B] =
    foldRight(s, Stream.empty[B])((el, acc) => cons(f(el), acc) )

  def findFirst[A](s:Stream[A])(p: A => Boolean):Option[A] =
    foldRight(s, Option.empty[A])((el, acc) => if (p(el)) Option(el) else acc)
  //headOption(filter(s)(p))

  def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] =
    foldRight(s, Stream.empty[A])((el, acc) => if (p(el)) cons(el, acc) else nil)
}

def test[A] (n:A)= {println("calcing " + n); n}

import Stream._
val s= cons(1, cons(2, cons(3, cons(4, nil))))
val ss = cons(test(1), cons(test(2), cons(test(3), cons(test(4), nil))))

toList(s)
headOption(s)
toList(take(ss, 2))

foldRight(s, 0)(_+_)
exists(s)(_ >100)
filter(ss)(_ < 4)
toList(map(s)(_ * 2))
forall(s)(_ < 3)
findFirst(s)(_ == 8)

val o = takeWhile(s)(_ < 3)
toList(o)

val a = map(ss)(_ * 10)
val b = map(a)(_ * 5)
val c = take(b, 1)
c.toString

val ones: Stream[Int] = cons(test(1), ones)
toList(take(ones, 10))

def constant(n: Int): Stream[Int] = cons(n, constant(n))
toList(take(constant(9), 10))

def start(n: Int): Stream[Int] = cons(n, start(n+1))

def zip[A, B](a: Stream[A], b: Stream[B]): Stream[(A, B)] = (a, b) match {
  case (SCons(ah, at), SCons(bh, bt)) => cons(ah() -> bh(), zip(at(), bt()))
  case _ => nil
}


val s1 = cons("A", cons("B", cons("C", cons("D", nil))))

def take2[A](s: Stream[A], n: Int): Stream[A] =
  takeWhile(s)(u => zip(u, start(0)))
  //takeWhile(map(zip(s, start(0)))(t => t._1))(u => {println(u); true})


toList(take2(s1, 3))