import lesson180904a.Input

case class State[S,+A](run: S => (A,S)) { // transition, action
  outer =>
  def map[B](f: A => B):State[S,B] = flatMap(a => State.unit[S](f(a)))

  def flatMap[B](f: A => State[S,B]):State[S,B] = State(s => {
    val (a, s2) = outer.run(s)
    f(a).run(s2)
  })

  def combine[B,C](sb:State[S,B])(f: (A,B) => C):State[S,C] = for {
    a <- outer
    b <- sb
  } yield f(a,b)
}

implicit class StateOps[S,A](s: State[S, State[S,A]]) {
  def flatten:State[S,A] = State.flatten(s)
}

object State {
  case class PartialAppliedState[S]() {
    def apply[A](v:A):State[S,A] = State(v -> _)
  }

  def unit[S]: PartialAppliedState[S] = PartialAppliedState[S]()

  def sequence[S,A](l:List[State[S,A]]):State[S,List[A]] = traverse(l)(identity)
  def traverse[S,A,B](l:List[A])(f: A => State[S,B]):State[S,List[B]] = l.foldRight(unit[S](List.empty[B])) {
    (el, acc) => combine(f(el), acc)(_ +: _)
  }
  def combine[S,A,B,C](sa:State[S,A], sb:State[S,B])(f: (A,B) => C):State[S,C] = sa.combine(sb)(f)

  def flatten[S,A](ssa:State[S, State[S, A]]):State[S,A] = ssa.flatMap(identity)

  def get[S]:State[S, S] = ???

  def set[S](s:S):State[S, Unit] = ???

  def modify[S](f: S => S):State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield()

}

object Random {
  import State._

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


  def int: Rand[Int] = State(_.nextInt)

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

{
  import Random._
  assoc(int, shortStr()).run(SimpleRNG(10))
}


case class Machine(locked:Boolean, candies:Int, coins:Int)

sealed trait Intpu
case object Coin extends Input
case object Turn extends Input

def simpleMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???