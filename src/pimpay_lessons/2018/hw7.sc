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

  def foldLeft[A, B](l:MyList[A], z: B)(f: (A, B) => B): B = l match {
    case MyCons(h, t) => foldLeft(t, f(h, z))(f)
    case MyNil => z
  }

  def dropWhile[A](l: MyList[A], p: A => Boolean): MyList[A] =
    foldRight(l, MyList[A])((i, a) => if (p(i)) a else append(MyList(i), a))

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    var z = n
    dropWhile(l, (i: A) => {z = z - 1; z >= 0})
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

  def hasSubsequence[A](haystack:MyList[A], needle:MyList[A]):Boolean = {
    val origNeedle = needle

    @annotation.tailrec
    def subSeq(sup: MyList[A], sub: MyList[A]): Boolean = (sup, sub) match {
        case (_, MyNil) => true
        case (MyCons(h1, t1), MyCons(h2, t2)) if h1 == h2 => subSeq(t1, t2)
        case (MyCons(h1, t1), MyCons(h2, t2)) if h1 != h2 && h2 == MyList.head(origNeedle) => subSeq(t1, origNeedle)
        case (MyCons(h1, t1), MyCons(h2, t2)) if h1 != h2 && h2 != MyList.head(origNeedle) => subSeq(sup, origNeedle)
        case _ => false
      }

    subSeq(haystack, needle)
  }

}


val a = MyList(1, 2, 3, 4)
val b = MyList(9, 8, 7, 6)
val c = MyList(a, b)
val d = MyList(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
MyList.head(a)
MyList.dropWhile(a, (x:Int) => x < 3)
MyList.dropWhile(d, (x: Int) => x < 14)
MyList.drop(a, 3)
MyList.drop(a, 1)
MyList.map(a, (i:Int) => i * 2)

MyList.append(a, b)
MyList.flatten(c)
MyList.flatMap(a, (x:Int) => MyList(x, x*x))
MyList.filter(a, (x:Int) => x % 2 == 0 )

MyList.exists(a, (x: Int) => x == 2)
MyList.reverse(b)

val haystack = MyList(1,2,3,4,5)


MyList.hasSubsequence(haystack, MyList(1,2,3,4,5))
MyList.hasSubsequence(haystack, MyList(2,3,4))
MyList.hasSubsequence(haystack, MyList(3,4,5))

MyList.hasSubsequence(haystack, MyList(2,3,5))
MyList.hasSubsequence(haystack, MyList(1,3,2))
MyList.hasSubsequence(haystack, MyList(1,2,3,4,5,6))
