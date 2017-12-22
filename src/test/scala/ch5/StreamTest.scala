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

  "take" should "handle negative n" in
    assert(Stream(1, 2, 3).take(-1) == Empty)

  it should "handle Empty" in {
    assert(Stream.empty.take(0) == Empty)
    assert(Stream.empty.take(1) == Empty)
  }

  it should "handle cases where n less than or equal to stream length" in {
    assert(Stream(1, 2, 3).take(0) == Stream.empty)
    assert(Stream(1, 2, 3).take(1).toList == List(1))
    assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
    assert(Stream(1, 2, 3).take(3).toList == List(1, 2, 3))
  }

  it should "handle cases where n greater than or equal to stream length" in
    assert(Stream(1, 2, 3).take(4).toList == List(1, 2, 3))

  "drop" should "handle negative n" in
    assert(Stream(1, 2, 3).drop(-1) == Empty)

  it should "handle Empty" in {
    assert(Stream.empty.drop(0) == Empty)
    assert(Stream.empty.drop(1) == Empty)
  }

  it should "handle cases where n less than or equal to stream length" in {
    assert(Stream(1, 2, 3).drop(0).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3).drop(1).toList == List(2, 3))
    assert(Stream(1, 2, 3).drop(2).toList == List(3))
    assert(Stream(1, 2, 3).drop(3) == Empty)
  }

  it should "handle cases where n greater than or equal to stream length" in
    assert(Stream(1, 2, 3).drop(4) == Empty)

  "takeWhile" should "handle Empty" in
    assert(Stream.empty[Int].takeWhile(_ => true) == Stream.empty)

  it should "handle non-empty streams" in {
    assert(Stream(1, 2, 3).takeWhile(_ => false).toList == Nil)
    assert(Stream(1, 2, 3).takeWhile(_ % 2 == 1).toList == List(1))
    assert(Stream(1, 2, 3).takeWhile(_ <= 2).toList == List(1, 2))
    assert(Stream(1, 2, 3).takeWhile(_ => true).toList == List(1, 2, 3))
  }
}
