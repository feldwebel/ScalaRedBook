/*val rng = scala.util.Random

rng.nextInt()
rng.nextInt()*/

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG
{
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    //val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    //(n, nextRNG)
    n -> SimpleRNG(newSeed)
  }
}

/*def nextInt(rng: RNG): (Int, RNG) = rng.nextInt

def nextNonNegativeInt(rng: RNG): (Int, RNG) = {
  val next = rng.nextInt
  if (next._1 > 0) next else (-(next._1 + 1), next._2)
}

def nextDouble(rng: RNG): (Double, RNG) = {
  val next = nextNonNegativeInt(rng)
  (next._1 / (Int.MaxValue.toDouble + 1), next._2)
}

def nextIntMax(rng: RNG, max: Int): (Int, RNG) = {
  val (a, r) = nextDouble(rng)
  (a * max).toInt -> r
}*/
type Rand[+A] = RNG => (A, RNG)



/*def nonNegativeInt:Rand[Int] = rng => {
  val next = rng.nextInt
  if (next._1 > 0) next else (-(next._1 + 1), next._2)
}
def nextDouble1: Rand[Double] = rng => {
  val next = nextNonNegativeInt(rng)
  (next._1 / (Int.MaxValue.toDouble + 1), next._2)
}*/

def rmap[A, B](ra: Rand[A])(f: A => B): Rand[B] = rng => {
  val (a, rng2) = ra(rng)
  f(a) -> rng2
}
def int:Rand[Int] = _.nextInt
def nonNegativeInt: Rand[Int] = rmap(int) (_.abs)

def double:Rand[Double] = rmap(nonNegativeInt)(_.toDouble/ Int.MaxValue)

def range(from:Int, to:Int):Rand[Int] = nonNegativeInt map {i => {
  require(to > from)
  val l = to - from
  from + (i % l)
}}

def boolean:Rand[Boolean] = nonNegativeInt map (_ % 2 == 1)
def char:Rand[Char] = range(97,122) map (_.toChar)

implicit class RandOps[A](ra: Rand[A] ){
  def map[B](f: A => B): Rand[B] = rmap(ra)(f)
}

val rng = new SimpleRNG(8124)

double(rng)
range(5, 10)(rng)

char(rng)

def twoInts:Rand[(Int, Int)] = rng => {
  val (i1, rng2) = int(rng)
  val (i2, rng3) = int(rng2)

  (i1, i2) -> rng3
}

def combine[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
  val (i1, rngA) = ra(rng)
  val (i2, rngB) = rb(rngA)

  f(i1, i2) -> rngB
}

def twoInts2: Rand[(Int, Int)] = combine(int, int)( _ -> _)

def tuplize[A, B](ra:Rand[A], rb:Rand[B]):Rand[(A,B)] = combine(ra, rb) (_ -> _)




case class Point(x:Int, y:Int)
def point1: Rand[Point] = rng => {
  val (x1, rng1) = int(rng)
  val (y1, rng2) = int(rng1)

  Point(x1, y1) -> rng2
}

def point: Rand[Point] = combine(int, int) (Point)

val z = point(rng)

def sequence[A](l:List[Rand[A]]): Rand[List[A]] = l.foldRight(unit(List.empty[A]))((el, acc) => combine(el, acc)(_ +: _))
//combine(_, _)(_ +: _)

def traverse[A, B](l: List[A])(f: A => Rand[B]): Rand[List[B]] =
  l.foldRight(unit(List.empty[B]))((el, acc) => combine(f(el), acc)(_ +: _))

def unit[A](a:A): Rand[A] = a -> _ // rng => (a, rng)

def fixedStr(l:Int):Rand[String] = sequence(List.fill(l)(char)) map (_.mkString)

def listOf[A](ra:Rand[A], n:Int): Rand[List[A]] = sequence(List.fill(n)(ra))
def fixed(l:Int):Rand[String] = listOf(char,l) map (_.mkString)

val zzz =fixedStr(66)(rng)


def shortStr(min:Int =2, max: Int = 15): Rand[String] = ??? // map.combine/sequence
//строка  случайной длины в пределах

