object lessDEADBEAF {
  val rng = scala.util.Random

  rng.nextDouble()
  rng.nextDouble()

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG { // linear congruential generator
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  val rng1 = SimpleRNG(1)
  rng1.nextInt
  rng1.nextInt
  SimpleRNG(1).nextInt
  SimpleRNG(1488).nextInt

  def randomPair(rng:RNG): ((Int, Int), RNG) = {
    val i1 = rng.nextInt
    val i2 = i1._2.nextInt
    ((i1._1, i2._1), i2._2)
  }

  randomPair(rng1)

  def nonNegativeInt(rng:RNG): (Int, RNG) = {
    val (i1, rng1)= rng.nextInt
    if (i1 < 0) (-i1, rng1) else (i1, rng1)
  }

  nonNegativeInt(rng1)

  def double(rng:RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)

    (i / (Int.MaxValue.toDouble), rng1)
  }

  double(rng1)

  def intDouble(rng:RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)
  }

  def double3(rng:RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count:Int)(rng:RNG): (List[Int], RNG) = {
    def ints1(count:Int, rng: RNG, l:List[Int]): (List[Int], RNG) = {
      if (count <= 0)
        (l, rng)
      else {
        val (i, rng1) = rng.nextInt
        ints1(count-1, rng1, i::l)
      }
    }

    ints1(count, rng, List())
  }

  ints(10)(rng1)

  def intsViaFR(count:Int)(rng:RNG): (List[Int], RNG) = {
    val l = List.fill(count)(0)
    l.foldRight((List.empty[Int], rng))((_, a) => {
      val (l1, rng) = a
      val (i, rng1) = rng.nextInt
      (i::l1, rng1)
    })
  }

  intsViaFR(10)(rng1) == ints(10)(rng1)


  type Rand[+A] = RNG => (A, RNG)

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](r:Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng1) = r(rng)
      (f(a), rng1)
    }

  def nonNegativeIntViaMap:Rand[Int] = {
    map(int)(i => if (i < 0) -i else i)
  }

  nonNegativeIntViaMap(rng1)

  def doubleViaMap:Rand[Double] =
    map(nonNegativeIntViaMap)(_ / Int.MaxValue.toDouble)

  doubleViaMap(rng1) == double(rng1)

  def randPair: Rand[(Int, Int)] = {
    product(int, int)
  }

  def map2[A,B,C](ra:Rand[A], rb: Rand[B])(f:(A, B) => C): Rand[C] = {
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }
  }

  def product[A,B](ra:Rand[A], rb:Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  randPair(rng1) == randomPair(rng1)
  
  }