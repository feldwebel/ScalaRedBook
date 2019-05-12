// streams
sealed trait Stream[+A] {
  override def toString = "Stream(<lambda>)"
}

case class SCons[A](h: () => A, t: () => Stream[A]) extends Stream[A] // thunk
case object SNil extends Stream[Nothing]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {

    lazy val hh = h
    lazy val tt = t

    SCons(() => hh, () => tt)
  }

  def nil = SNil

  def empty[A]: Stream[A] = SNil

  def apply[A](a: A*):Stream[A] = if (a.isEmpty) empty[A] else cons(a.head, apply(a.tail:_*))

  def toList[A](s: Stream[A]): List[A] = s match {
    case SCons(h, t) => h() +: toList(t())
    case SNil => Nil
  }

  def headOption[A](s:Stream[A]):Option[A] = foldRight(s, Option.empty[A]){ (el, _) => Option(el) }

  def map[A,B](s:Stream[A])(f: A => B):Stream[B] = unfold(s) {
    case SCons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def filter[A](s:Stream[A])(p: A => Boolean): Stream[A] = foldRight(s, empty[A]){ (el, acc) =>
    if (p(el)) cons(el, acc) else acc
  }

  def constant(n: Int): Stream[Int] = unfold( () )(_ => Option(n, ()))
  def start(n:Int):Stream[Int] = unfold(n)(s => Option(s, s + 1))
  def fibs:Stream[Int] = unfold(0 -> 1){ case (a,b) => Option(a, (b, a + b)) }

  def unfold[A,S](z:S)(f: S => Option[(A,S)]):Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => nil
  }

  // 0 1 1 2 3 5 8 13 21

  def zip[A,B](a:Stream[A], b:Stream[B]):Stream[(A,B)] = (a,b) match {
    case (SCons(ah, at), SCons(bh, bt)) => Stream.cons(ah() -> bh(), zip(at(), bt()))
    case _ => Stream.nil
  }

  def zipWith[A,B,C](a:Stream[A], b:Stream[B])(f: (A,B) => C):Stream[C] = map (zip(a,b)) (f.tupled)
  def zipWith2[A,B,C](a:Stream[A], b:Stream[B])(f: (A,B) => C):Stream[C] = ??? //unfold

  def zipWithIndex[A](s:Stream[A]):Stream[(A,Int)] = mapAccum(s, 0) { (a, idx) => (a -> idx, idx + 1) }

  def take[A](s: Stream[A], n:Int):Stream[A] = map (takeWhile(zipWithIndex(s)) { _._2 < n }) { _._1 }

  def mapAccum[A,B,S](s:Stream[A], z:S)(f: (A,S) => (B,S)):Stream[B] = {
    var st = z
    foldRight(s, empty[B]) { (el, acc) => {
      val (b, ns) = f(el, st)
      st = ns
      cons(b, acc)
    }}
  }

  def takeWhile[A](s:Stream[A])(p: A => Boolean):Stream[A] = foldRight(s, empty[A]){ (el, acc) =>
    if (p(el)) cons(el, acc) else nil
  }


  def exists[A](s:Stream[A])(p: A => Boolean):Boolean = foldRight(s, false)((el,acc) => p(el) || acc)
  def forall[A](s:Stream[A])(p: A => Boolean):Boolean = foldRight(s, true)((el,acc)  => p(el) && acc)

  def findFirst[A](s:Stream[A])(p: A => Boolean):Option[A] = headOption(filter(s)(p))


  def foldRight[A,B](s: Stream[A], z: B)(f: (A, =>B) => B):B = s match {
    case SCons(h, t) => f(h(), foldRight(t(), z)(f))
    case SNil => z
  }

  def sum(s:Stream[Double]):Double = foldRight(s, .0)(_+_)

  /*
Д.З.:
1) hasSubsequene для стримов через tails/exists/startWith(isPrefixOf) как в Listах

Vladimir Ayupov, [26.04.19 14:07]
2) как можно больше функций в Stream через unfold: map,zip*,takeWhile
   */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forall {case(h1, h2) => h1 == h2}

  def tails: Stream[Stream[A]] = unfold(this) {
    case nil => None
    case s => Some((s, s drop 1))
  } append Stream(Stream.empty)

  def hasSubsequence[A](haystack:Stream[A], needle:Stream[A]):Boolean =
    exists( tails(haystack) )  { _.startsWith(needle) }
}


import Stream._

def test[A](n: A): A = {
  println("calcing " + n); n
}

val ss = cons(test("a"), cons(test("b"), cons(test("c"), cons(test("d"), nil))))

map(ss){ _.length }

