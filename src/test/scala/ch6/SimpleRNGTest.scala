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
    val (n1, rng2) = SimpleRNG.nonNegativeInt.run(rng)
    assert { n1 == 16159453 }
    val (n2, _) = SimpleRNG.nonNegativeInt.run(rng2)
    assert { n2 == 1281479697 }
  }

  "SimpleRNG.double" should "generate doubles between 0 and 1, exclusive" in {
    val (d1, rng2) = SimpleRNG.double.run(rng)
    assert { d1 == 0.007524831689672932 }
    val (d2, rng3) = SimpleRNG.double.run(rng2)
    assert { d2 == 0.5967354856416283 }
  }

  "SimpleRNG.ints" should "handle negative count" in
    assert { SimpleRNG.ints(-1).run(rng)._1 == Nil }

  it should "handle count of 0" in
    assert { SimpleRNG.ints(0).run(rng)._1 == Nil }

  it should "handle positive counts" in {
    assert { SimpleRNG.ints(1).run(rng)._1 == List(16159453) }
    assert { SimpleRNG.ints(2).run(rng)._1 == List(16159453, -1281479697) }
  }

  "SimpleRNG.nonNegativeLessThan" should "generate appropriate random values" in {
    val lRand = List.fill(10) { SimpleRNG.nonNegativeLessThan(4) }
    val rList = State.sequence(lRand)
    assert{ rList.run(rng)._1 == List(1, 1, 2, 0, 2, 3, 0, 1, 3, 2) }
  }

  it should "produce a uniform distribution" in {
    // Replaces Int.MaxVal with a low value in order to show that logic that ensures
    //   uniform distribution is exercised.
    val CustomIntMaxVal = 20
    val lRand = List.fill(10) { SimpleRNG.nonNegativeLessThan(4, CustomIntMaxVal) }
    val rList = State.sequence(lRand)
    // Notice different output than above test; the lower max was exceeded, and
    //   the uniformity logic was exercised!
    assert{ rList.run(rng)._1 == List(1, 0, 3, 1, 2, 0, 3, 1, 1, 2) }
  }
}
