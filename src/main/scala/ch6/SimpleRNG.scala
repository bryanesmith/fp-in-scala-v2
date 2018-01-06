package ch6

import ch6.Rand._


case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val n = (newSeed >>> 16).toInt
    (n, SimpleRNG(newSeed))
  }
}

object SimpleRNG {

  val int: Rand[Int] = _.nextInt

  // Exercise 6.1
  @annotation.tailrec
  val nonNegativeInt: Rand[Int] = (rng: RNG) => rng.nextInt match {
    case(i, rng2) if i == Int.MinValue => nonNegativeInt(rng2)
    case(i, rng2) => (Math.abs(i), rng2)
  }
  
  // Exercise 6.2, 6.5
  def double: Rand[Double] =
    map(nonNegativeInt) { i => i.toDouble / Int.MaxValue} // TODO: can equal 1

  // Exercise 6.3
  val intDouble: Rand[(Int, Double)] = both(int, double)

  // Exercise 6.3
  val doubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 6.3
  val double3: Rand[(Double,Double,Double)] = (rng: RNG) => {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // Exercise 6.4, 6.7
  def ints(c: Int): Rand[List[Int]] = sequence { List.fill(c)(int) }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

}
