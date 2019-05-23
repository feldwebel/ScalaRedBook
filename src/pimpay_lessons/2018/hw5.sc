def factorial(n:Int): BigInt = {
  def f(n: Int, fact: BigInt): BigInt = {
    if (n == 1) fact else f(n-1, fact * n)
  }

  f(n, 1)
}

factorial(3) // 6
factorial(5) // 120
factorial(10) // 3628800


def isSorted[A](as:Array[A])(cmp: (A,A) => Boolean):Boolean = {
  @annotation.tailrec
  def s(n: Int, acc: Boolean): Boolean = {
    val r = acc && cmp(as(n-1), as(n))
    if (r != acc) false
    else if (n == 1) r
    else s(n - 1, r)
  }

  if (as.length < 3) true else {
    s(as.length - 2, cmp(as(as.length-2), as(as.length-1)))
  }
}

val array = Array(1,2,4)
val ar2 = Array(3, 8, 0, 4)
val ar3 = Array("a", "b", "c", "d")

isSorted(Array(1, 2)) {_ < _}

array(0)
array(1)
array(2)

array.length

isSorted(array){_ < _} // true
isSorted(ar2){_ <= _}   // false
isSorted(ar3){_ <= _}   // true
