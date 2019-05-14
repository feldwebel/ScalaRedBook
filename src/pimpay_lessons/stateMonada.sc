val rng = scala.util.Random

rng.nextInt()
rng.nextInt()

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

def nextInt(rng: RNG): (Int, RNG) = rng.nextInt

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
}
type Rand[+A] = RNG => (A, RNG)

def nextInt1:Rand[Int] = rng => rng.nextInt
def nextNonNegativeInt1:Rand[Int] = rng => {
  val next = rng.nextInt
  if (next._1 > 0) next else (-(next._1 + 1), next._2)
}
def nextDouble1: Rand[Double] = rng => {
  val next = nextNonNegativeInt(rng)
  (next._1 / (Int.MaxValue.toDouble + 1), next._2)
}

def map[A, B](ra: Rand[A])(f: A => B): Rand[B] = ???