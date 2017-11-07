object MyChapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](t: List[A]): List[A] = t match {
      case Nil => Nil
      case Cons(_, t1) => t1
    }

    def setHead[A](a: A, l: List[A]): List[A] = l match {
      case Nil => Cons(a, Nil)
      case Cons(_, t) => Cons(a, t)
    }

    def drop[A](l: List[A], n: Int): List[A] = n match {
      case 0 => l
      case _ => drop(tail(l), n-1)
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => List.dropWhile(t, f)
      case _ => l
    }

  }

  val a = List(1, 2, 3, 4, 5)
  val b = List(88)

  List.tail(a)
  List.tail(b)

  List.setHead(8, a)

  List.drop(a, 2)

  List.dropWhile(a, (x: Int) => x < 3)


}