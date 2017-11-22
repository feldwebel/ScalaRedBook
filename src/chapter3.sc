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
      foldRight(first, second)((h, acc) => Cons(h, acc))

    def concatenate[A](as: List[List[A]]): List[A] =
      foldRight(as, Nil:List[A])(append)

    def incOne(as: List[Int]): List[Int] =
      foldRight(as, Nil:List[Int])((h, acc) => Cons(h+1, acc))

    def stringify(as: List[Double]): List[String] =
      foldRight(as, Nil:List[String])((h, acc) => Cons(h.toString, acc))

    def map[A,B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil:List[B])((h, t) => Cons(f(h), t))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
      concatenate(map(as)(f))

    def filter1[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)((a) => if (f(a)) List(a) else Nil)

    def coupling(first: List[Int], second: List[Int]): List[Int] =
      (first, second) match {
        case (_, Nil) => Nil
        case (Nil, _) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, coupling(t1, t2))
      }

    def zipWith[A,B](first: List[A], second: List[A])(f: (A, A) => B ): List[B] =
      (first, second) match {
        case (_, Nil) => Nil
        case (Nil, _) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
      ???

  }

  val a = List(1, 2, 3, 4, 5)
  val b = List(88)
  val c = List(List(11, 22, 33), List(44, 55, 66))
  val d = List(1.5, 2.71, 3.14)

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

  List.concatenate(c)

  List.incOne(a)

  List.stringify(d)

  List.map(a)((i: Int) => i + 1)

  List.filter(a)((i: Int) => i > 2)

  List.flatMap(List(1,2,3))(i => List(i,i))

  List.filter1(a)((i: Int) => i > 2)

  List.coupling(List(1, 2, 3), List(4, 5, 6))

  List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _)

  List.hasSubsequence(a, List(2, 3))

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](t: Tree[A]): Int =
      t match {
        case Leaf(_) => 1
        case Tree(left, right) => 1 + size(left) + size(right)
      }
  }

  val t1 = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(4)))


}

