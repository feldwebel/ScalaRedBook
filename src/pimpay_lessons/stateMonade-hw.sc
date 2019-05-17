case class State[S,+A](run: S => (A,S)) {
  def map[B]
  def flatMap[B]
  def combine[B,C]
}

object State {
  def unit[A]
  def sequence[A]
  def traverse[A,B]
  def combine[A,B,C]
}

object Random {
  type Rand[A] = State[RNG, A]

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }


  def int: Rand[Int] = _.nextInt

  def nonNegativeInt: Rand[Int] = int map {
    _.abs
  }

  def double: Rand[Double] = nonNegativeInt map {
    _.toDouble / Int.MaxValue
  }

  def range(from: Int, to: Int): Rand[Int] = nonNegativeInt map { i => {
    require(to > from)
    val l = to - from
    from + (i % l)
  }
  }

  def boolean: Rand[Boolean] = nonNegativeInt map {
    _ % 2 == 1
  }

  def char: Rand[Char] = range(97, 122) map {
    _.toChar
  }

  def tuple[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = combine(ra, rb) {
    _ -> _
  }

  case class Point(x: Int, y: Int)

  def point: Rand[Point] = combine(range(0, 10), range(-10, 20))(Point)

  def listOf[A](ra: Rand[A], n: Int): Rand[List[A]] = sequence(List.fill(n)(ra))

  def fixedStr(l: Int): Rand[String] = listOf(char, l) map (_.mkString)

  def shortStr(min: Int = 2, max: Int = 15): Rand[String] = for {
    l <- range(min, max)
    s <- fixedStr(l)
  } yield s


  def assoc[K, V](rk: Rand[K], rv: Rand[V], rn: Rand[Int] = range(2, 10)): Rand[Map[K, V]] = for {
    n <- rn
    l <- listOf(tuple(rk, rv), n)
  } yield l toMap

  val rng = SimpleRNG(10)

  assoc(shortStr(), assoc(int, int)).run(rng)
}