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
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil | Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(h, t) => f(h, foldRight(t, z)(f))
      }

    def length[A](as: List[A]): Int =
      foldRight(as, 0)((_, a) => a + 1)

    @annotation.tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }

    def sumLeft(as: List[Int]) =
      foldLeft(as, 0)((x, y) => x + y)

    def productLeft(as: List[Int]) =
      foldLeft(as, 1.0)((x, y) => x * y)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, List[A]())((a, h) => Cons(h, a))

    def append[A](first: List[A], second: List[A]): List[A] =
      foldLeft(second, first)((acc, h) => Cons(h, acc))

  }

  val a = List(1, 2, 3, 4, 5)
  val b = List(88)

  List.tail(a)
  List.tail(b)

  List.setHead(8, a)

  List.drop(a, 2)

  List.dropWhile(a, (x: Int) => x < 3)

  List.init(a)

  List.length(a)

  List.foldLeft(a, 0)((x: Int, y: Int) => x + y)

  List.sumLeft(a)

  List.productLeft(a)

  List.reverse(a)

  List.append(a, b)


}
