package ch6

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val n = (newSeed >>> 16).toInt
    (n, SimpleRNG(newSeed))
  }
}

object SimpleRNG {
  // Exercise 6.1
  @annotation.tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case(i, rng2) if i == Int.MinValue => nonNegativeInt(rng2)
    case(i, rng2) => (Math.abs(i), rng2)
  }
}
