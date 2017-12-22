package ch5

import org.scalatest.FlatSpec

class StreamTest extends FlatSpec {
  "toList" should "handle empty" in
    assert(Stream.empty.toList == Nil)

  it should "handle non-empty" in {
    assert(Stream("a").toList == List("a"))
    assert(Stream("a", "b").toList == List("a", "b"))
    assert(Stream("a", "b", "c").toList == List("a", "b", "c"))
  }
}
