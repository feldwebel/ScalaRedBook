case class State[S,+A](run: S => (A,S)) {
  def map[B](f:A => B): State[S,B] = flatMap(a => State.unit(f(a)))
  def flatMap[B](f:A => State[S,B]):State[S,B] = State(s => {
      val (a, s1) = run(s)
      f(a) run s1
  })
  def combine[B,C](sb: State[S, B]): State[S,C] = ???
}

object State {
  def unit[S,A](a:A):State[S,A] = State(s => (a, s))

  def sequence[S,A](l:List[State[S, A]]): State[S, List[A]] =
    l.foldRight(unit[S, List[A]](List.empty[A]))((el, acc) => combine(el, acc)(_ +: _))

  def traverse[S,A,B](l: List[A])(f: A => State[S,B]): State[S, List[B]] =
    l.foldRight(unit[S, List[A]](List.empty[A]))((el, acc) => combine(f(el), acc)(_ +: _))

  def combine[S,A,B,C](ra:State[S,A], rb:State[S,B])(f:(A, B) => C): State[S,C] =
    for {
      a <- ra
      b <- rb
    } yield f(a, b)
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