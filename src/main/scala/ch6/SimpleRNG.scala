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

  // Exercise 6.2
  @annotation.tailrec
  def double(rng:RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (i, rng2) if i == Int.MaxValue => double(rng2)
    case (i, rng2) => (i.toDouble / Int.MaxValue, rng2)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  // Exercise 6.3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  // Exercise 6.3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }
}
