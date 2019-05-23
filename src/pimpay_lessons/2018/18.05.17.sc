object lesson1 {
  val x = 2.toString

  def u() = {}

  def f(x: Int) = {
    println(x * 2); x + 2
  }

  f(2)

  val fff = if (true) 2 else "0".toInt

  f(fff)

  def min(a: Int, b: Int): Int = {
    println("Int")
    if (a > b) b else a
  }

  def min(a: Double, b: Double): Double = {
    println("double")
    if (a > b) b else a
  }



  min(6, 3)

  min(1.5, 6)

  def lt(a: Int, b: Int) = if (a < b) true else false

  val seq = Seq(1, 2, 3, 0)

  seq.sortWith(lt)

  def factorFactory(k: Int): Int => Int = (a: Int) => {
    a * k
  }


  factorFactory(6)(3)

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)
  //f(_)(_)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def mul(a: Int, b: Int) = a * b

  def mul1(a: Int)(b: Int) = a * b

  curry(mul)(6)(3) // 18
  mul(6, 3) == curry(mul)(6)(3) //true

  uncurry(mul1)(6, 3) // 18

  mul1(6)(3) == uncurry(mul1)(6, 3)


  def f3(a: Int) = a + 2
  def f4(b: Int) = b * 2

  compose(f3, f4)(3) // (3 * 2) + 2
  compose(f4, f3)(3) // (3 + 2) * 2

  mul(2, 3) == 6

  val mulC = curry(mul)

  mulC(2)(3) == 6

}