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
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, coupling(t1, t2))
        case _ => Nil
      }

    def zipWith[A,B](first: List[A], second: List[A])(f: (A, A) => B ): List[B] =
      (first, second) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
        case _ => Nil
      }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      val origSub = sub
      def hd[A](l: List[A]) = l match {
        case Cons(h, _) => h
        case _ => null
      }
      
      @annotation.tailrec
      def subSeq(sup: List[A], sub: List[A]): Boolean =
        (sup, sub) match {
          case (_, Nil) => true
          case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => subSeq(t1, t2)
          case (Cons(h1, t1), Cons(h2, t2)) if (h1 != h2 && h2 == hd(origSub)) => subSeq(t1, origSub)
          case (Cons(h1, t1), Cons(h2, t2)) if (h1 != h2 && h2 != hd(origSub)) => subSeq(sup, origSub)
          case _ => false
        }

      subSeq(sup, sub)
    }
  }

  val a = List(1, 2, 3, 4, 5)
  val b = List(88)
  val c = List(List(11, 22, 33), List(44, 55, 66))
  val d = List(1.5, 2.71, 3.14)

  List.hasSubsequence(a, List(2, 3))
  List.hasSubsequence(a, List(14, 88))
  List.hasSubsequence(a, List(4, 5, 6))
  List.hasSubsequence(List(1,1,2,3), List(1,2,3))

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

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def apply[A](a: A): Tree[A] = Leaf(a)
    def apply[A](a: Tree[A], b: Tree[A]): Tree[A] = Branch(a, b)

    def size[A](t: Tree[A]): Int =
      t match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + size(left) + size(right)
      }

    def maximum[A](t: Tree[A]): Int =
      t match {
        case Leaf(x) => x.toString.toInt
        case Branch(left, right) => {
          /*val l = maximum(left)
          val r = maximum(right)
          if (l > r) l else r*/
          maximum(left) max maximum(right)
        }
      }

    def depth[A](t: Tree[A]):Int =
      t match {
        case Leaf(_) => 1
        case Branch(left, right) => {
          /*val l = depth(left)
          val r = depth(right)
          if (l > r) l + 1 else r + 1*/
          (depth(left) max depth(right)) + 1
        }
      }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      t match {
        case Leaf(x) => Leaf(f(x))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }

    def fold[A,B](t: Tree[A])(f: A => B)(f2: (B, B) => B): B =
      t match {
        case Leaf(x) => f(x)
        case Branch(left, right) => f2(fold(left)(f)(f2), fold(right)(f)(f2))
      }

    def mapViaFold[A,B](t: Tree[A])(g:A => B): Tree[B] =
      fold(t)(x => Leaf(g(x)): Tree[B])((x1, x2) => Branch(x1, x2))
  }

  val t1 = Tree(Tree(Tree(Tree(1), Tree(22)), Tree(4)), Tree(3))

  Tree.size(t1)

  Tree.maximum(t1)

  Tree.depth(t1)

  Tree.map(t1)(i => i + 1)

  Tree.fold(t1)(x => 1)((x1, x2) => 1 + x1 + x2)//size

  Tree.fold(t1)(x => x.toString.toInt)((x1, x2) => x1 max x2)//maximum

  Tree.fold(t1)(x => 1)((x1, x2) => (x1 max x2) + 1)//depth

  Tree.mapViaFold(t1)(i => i + 1)

}

