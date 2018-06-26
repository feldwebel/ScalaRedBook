
sealed trait MyList[+A]

case object MyNil extends MyList[Nothing]

case class MyCons[A](head: A, tail: MyList[A]) extends MyList[A]

val a = MyCons(1, MyCons(2, MyCons(3, MyNil)))

object MyList {
  def sum(ls:MyList[Int]): Int = ls match {
    case MyCons(h, t) => h + sum(t)
    case MyNil => 0
  }

  def mul(ls:MyList[Int]): Int = ls match {
    case MyCons(h, t) => h * mul(t)
    case MyNil => 1
  }

  def apply[A](as:A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail:_*))

  def head[A](ls: MyList[A]): A = ls match {
    case MyCons(h, t) => h
    case MyNil => throw new Exception
  }

  def tail[A](ls: MyList[A]): MyList[A] = ls match {
    case MyCons(h, t) => t
    case MyNil => throw new Exception
  }

  def setHead[A](ls: MyList[A], nh: A): MyList[A] = ls match {
    case MyCons(h, t) => MyCons(nh, t)
    case MyNil => throw new Exception
  }

  def append[A](a: MyList[A], b: MyList[A]): MyList[A] = a match {
    case MyCons(h, t) => MyCons(h, append(t, b))
    case MyNil => b
  }

  def drop[A](l: MyList[A], n: Int): MyList[A] = l match {
    case MyCons(h, t) => if (n == 0) l else drop(t, n-1)
    case MyNil => MyNil
  }

  def dropWhile[A](l: MyList[A], p: A => Boolean): MyList[A] = l match {
    case MyCons(h, t) => if (!p(h)) l else dropWhile(t, p)
    case MyNil => MyNil
  }

  def drop1[A](l: MyList[A], n: Int): MyList[A] = {
    dropWhile(l, ???)
  }

  def foldRight[A,B](l: MyList[A], z: B)(f:(A, B) => B): B = l match {
    case MyCons(h, t) => f(h, foldRight(t, z)(f))
    case MyNil => z
  }

  def sum1(l:MyList[Int]): Int = {
    foldRight(l, 0)(_ + _)
  }

  def mul1(l: MyList[Int]): Int = {
    foldRight(l, 1)(_ * _)
  }

  def size[A](l: MyList[A]): Int = {
    foldRight(l, 0)((_, n) => n + 1)
  }

  def map[A,B](l:MyList[A], f: A => B):MyList[B] = l match {
    case MyCons(h, t) => MyCons(f(h), map(t, f))
    case MyNil => MyNil
  }

  // 1. Все функции переделать на foldRight

  // 2. Определить следующие (на foldRight):

  def map[A,B](ls: MyList[A], f: A => B):MyList[B] = ???
  def flatten[A](lss:MyList[MyList[A]]):MyList[A] = ???
  def flatMap[A,B](ls:MyList[A], f: A => MyList[B]):MyList[B] = ???
  def filter[A](ls:MyList[A], p: A => Boolean):MyList[A] = ???
  def exists[A](ls:MyList[A], p: A => Boolean):Boolean = ???
  def reverse[A](ls:MyList[A]):MyList[A] = ???


  // 3. Есть ли полное подвхождение needle внутри haystack
  def hasSubsequece[A](haystack:MyList[A], needle:MyList[A]):Boolean = ???

  // haystack = MyList(1,2,3,4,5)
  // hasSubsequences: MyList(1,2,3,4,5), MyList(2,3,4), MyList(3,4,5)
  // doesnt have subsequence: MyList(2,3,5), MyList(1,3,2), MyList(1,2,3,4,5,6)
}
MyList.sum(a)
MyList.mul(a)

val b = MyList(1, 2, 3, 4)

MyList.tail(b)
MyList.setHead(b, 88)

MyList.append(a, b)

MyList.drop(b, 2)
MyList.dropWhile(b, ((x: Int) => x < 2))
MyList.foldRight(a, 0)(_ + _)

MyList.sum1(b)
MyList.mul1(b)

MyList.size(b)

MyList.map(b, ((x:Int) => x * 2))



