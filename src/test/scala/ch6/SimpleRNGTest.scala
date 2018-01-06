package ch6

import org.scalatest.FlatSpec

class SimpleRNGTest extends FlatSpec {
  val rng = SimpleRNG(42)

  "SimpleRNG" should "generate expected numbers, given specific seed" in {
    val (n1, rng2) = rng.nextInt
    assert { n1 == 16159453 }
    val (n2, _) = rng2.nextInt
    assert { n2 == -1281479697 }
  }

  "SimpleRNG.nonNegativeInt" should "generate non-negative numbers" in {
    val (n1, rng2) = SimpleRNG.nonNegativeInt(rng)
    assert { n1 == 16159453 }
    val (n2, _) = SimpleRNG.nonNegativeInt(rng2)
    assert { n2 == 1281479697 }
  }

  "SimpleRNG.double" should "generate doubles between 0 and 1, exclusive" in {
    val (d1, rng2) = SimpleRNG.double(rng)
    assert { d1 == 0.007524831689672932 }
    val (d2, rng3) = SimpleRNG.double(rng2)
    assert { d2 == 0.5967354856416283 }
  }

  "SimpleRNG.ints" should "handle negative count" in
    assert { SimpleRNG.ints(-1)(rng)._1 == Nil }

  "SimpleRNG.ints" should "handle count of 0" in
    assert { SimpleRNG.ints(0)(rng)._1 == Nil }

  "SimpleRNG.ints" should "handle positive counts" in {
    assert { SimpleRNG.ints(1)(rng)._1 == List(16159453) }
    assert { SimpleRNG.ints(2)(rng)._1 == List(16159453, -1281479697) }
  }
}
