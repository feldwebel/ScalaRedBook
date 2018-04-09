object MyChapter6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG
  {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i < 0) (-(i + 1), r) else (i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (j, q) = double(r)
    ((i, j), q)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, j), r) = intDouble(rng)
    ((j, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (a, r1) = double(rng)
    val (b, r2) = double(r1)
    val (c, r3) = double(r2)
    ((a, b, c), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def ints1(count: Int, r: RNG, l: List[Int]): (List[Int], RNG) = {
      if (count <= 0) {
        (l, r)
      } else {
        val (a, r1) = r.nextInt
        ints1(count-1, r1, a :: l)
      }
    }
    ints1(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }


  val r1 = SimpleRNG(42)
val (a, r2) = r1.nextInt
val (b, r3) = nonNegativeInt(r2)
val (c, r4) = nonNegativeInt(r3)
val (d, r5) = nonNegativeInt(r3)
  val (e, r6) = double(r1)

  intDouble(r1)
  doubleInt(r1)
  double3(r1)

  ints(3)(r1)

  doubleMap(r1) == double(r1) //true

  map2(int, int)(_ + _)

}