object less04 {
  @annotation.tailrec
  def euclid(a: Int, b: Int): Int = //method type must be set
    if (b == 0) a else euclid(b, a % b)


  def gcd(aa: Int, bb: Int) = {
    var a = aa
    var b = bb

    while (b != 0) {
      val tmp = a % b
      a = b
      b = tmp
    }
    a
  }

  gcd(4, 10)
  euclid(4, 10)

  def fib(n: Int): Int = {
    @annotation.tailrec
    def f(n: Int, a: Int, b: Int):Int =
      if (n == 1) a else f(n - 1, b, a + b)

    f(n, 0, 1)
  }
  // 0, 1, 1, 2, 3, 5, 8, 13
  fib(1)
  fib(2)
  fib(4)
  fib(6)
  fib(7)

}