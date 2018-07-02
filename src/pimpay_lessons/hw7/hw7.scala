package pimpay_lessons.hw7

sealed trait MyList[+A]

case object MyNil extends MyList[Nothing]

case class MyCons[A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))

  def apply[A]:MyList[A] = MyNil

  def head[A](ls: MyList[A]): A = ls match {
    case MyCons(h, t) => h
    case MyNil => throw new Exception
  }

  def tail[A](ls: MyList[A]): MyList[A] = ls match {
    case MyCons(h, t) => t
    case MyNil => throw new Exception
  }

  def foldRight[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = l match {
    case MyCons(h, t) => f(h, foldRight(t, z)(f))
    case MyNil => z
  }

  def dropWhile[A](l: MyList[A], p: A => Boolean): MyList[A] =
    foldRight(l, MyList[A])((i, a) => if (p(i)) a else MyCons(i, a))

  def drop[A](l: MyList[A], n: Int): MyList[A] = { //if (n == 0) l else drop(t, n-1)
    dropWhile(l, (i:A) => n - 1 == 0)
  }

  def sum(l: MyList[Int]): Int = {
    foldRight(l, 0)(_ + _)
  }

  def mul(l: MyList[Int]): Int = {
    foldRight(l, 1)(_ * _)
  }

  def size[A](l: MyList[A]): Int = {
    foldRight(l, 0)((_, n) => n + 1)
  }

  def map[A,B](ls: MyList[A], f: A => B):MyList[B] =
    foldRight(ls, MyList[B])((i, a) => MyCons(f(i), a))

  def append[A](a: MyList[A], b: MyList[A]): MyList[A] =
    foldRight(a, b)((i, b) => MyCons(i, b))//MyCons(h, append(t, b))

  def flatten[A](lss:MyList[MyList[A]]):MyList[A] =
    foldRight(lss, MyList[A])((i, a) => append(i, a))

  def flatMap[A,B](ls:MyList[A], f: A => MyList[B]):MyList[B] =
    foldRight(ls, MyList[B])((i, a) => append(f(i), a))

  def filter[A](ls:MyList[A], p: A => Boolean):MyList[A] =
    foldRight(ls, MyList[A])((i, a) => if (p(i)) MyCons(i, a) else a)

  def exists[A](ls:MyList[A], p: A => Boolean):Boolean =
    foldRight(ls, true)((i, r) => r || p(i))

  def reverse[A](ls:MyList[A]):MyList[A] =
    foldRight(ls, MyList[A])((i, a) => append(a, MyList(i)))

}



