def fib(n: Int): Int = {
  def f1(n:Int, a: Int, b: Int): Int = n match {
    case 0 => a
    case 1 => b
    case _ => f1(n - 1, b, a + b)
  }

  f1(n, 0, 1)
}

fib(5) //5
fib(6) //8
fib(7) //13

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  def f1(n: Int): Boolean = {
    if (n >= as.length - 1) true
    else if (!ordered(as(n), as(n + 1))) false
    else f1(n+1)
  }
  f1(0)
}

isSorted(Array(1, 2, 3, 4, 5), (x:Int, y:Int) => x <= y) //true
isSorted(Array(1, 2, 3, 4, 3), (x:Int, y:Int) => x <= y) //false


def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a: A, b: B)

def f1(a: Int, b: Int): Int = a / b

curry(f1)(6)(3) // 6 / 3

f1(6, 3) == curry(f1)(6)(3) //true


def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

def f2(a: Int)(b: Int): Int = a / b

uncurry(f2)(6, 3) // 6 / 3

f2(6)(3) == uncurry(f2)(6, 3)


def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a: A))

def f3(a: Int) = a + 2
def f4(b: Int) = b * 2

compose(f3, f4)(3) // (3 * 2) + 2
compose(f4, f3)(3) // (3 + 2) * 2