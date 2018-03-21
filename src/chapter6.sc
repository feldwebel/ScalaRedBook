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

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i, r)
    }

    def double(rng: RNG): (Double, RNG)


  }

val r1 = SimpleRNG(42)
val (a, r2) = r1.nextInt
val (b, r3) = r1.nonNegativeInt(r2)
val (c, r4) = r1.nonNegativeInt(r3)
val (d, r5) = r1.nonNegativeInt(r3)




}