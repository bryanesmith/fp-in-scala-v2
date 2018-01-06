package ch6

import org.scalatest.FlatSpec

class SimpleRNGTest extends FlatSpec {
  "SimpleRNG" should "generate expected numbers, given specific seed" in {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    assert(n1 == 16159453)
    val (n2, _) = rng2.nextInt
    assert(n2 == -1281479697)
  }
}
