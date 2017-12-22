package ch5

import ch5.Stream._
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
    assert(Stream(1, 2, 3).takeWhile(_ != 1).toList == Nil)
    assert(Stream(1, 2, 3).takeWhile(_ % 2 == 1).toList == List(1))
    assert(Stream(1, 2, 3).takeWhile(_ <= 2).toList == List(1, 2))
    assert(Stream(1, 2, 3).takeWhile(_ => true).toList == List(1, 2, 3))
  }

  "contains" should "handle empty streams" in
    assert(!Stream.empty[Int].contains(1))

  it should "return false when non-empty stream doesn't contain element" in
    assert(!Stream(1, 2, 3).contains(4))

  it should "return true when non-empty stream contains element" in {
    assert(Stream(1, 2, 3).contains(1))
    assert(Stream(1, 2, 3).contains(2))
    assert(Stream(1, 2, 3).contains(3))
  }

  "exists" should "handle empty streams" in
    assert(!Stream.empty[Int].exists(_ => true))

  it should "return false when non-empty stream doesn't match predicate" in
    assert(!Stream(1, 2, 3).exists(_ == 4))

  it should "return true when non-empty stream matches predicate" in {
    assert(Stream(1, 2, 3).exists(_ == 1))
    assert(Stream(1, 2, 3).exists(_ == 2))
    assert(Stream(1, 2, 3).exists(_ == 3))
  }

  "find" should "handle empty streams" in
    assert(Stream.empty[Int].find(_ => true).isEmpty)

  it should "return None when non-empty stream doesn't match predicate" in
    assert(Stream(1, 2, 3).find(_ == 4).isEmpty)

  it should "return Some when non-empty stream matches predicate" in {
    assert(Stream(1, 2, 3).find(_ == 1).contains(1))
    assert(Stream(1, 2, 3).find(_ == 2).contains(2))
    assert(Stream(1, 2, 3).find(_ == 3).contains(3))
  }

  "foldRight" should "handle empty streams" in
    assert(Stream.empty[Int].foldRight(0)(_ + _) == 0)

  it should "handle non-empty streams" in
    assert(Stream(1, 2, 3).foldRight(0)(_ + _) == 6)

  "forAll" should "handle empty streams" in
    assert(Stream.empty[Int].forAll(_ => false))

  it should "return false for some non-empty streams" in
    assert(!Stream(1, 2, 3, 4, 5).forAll(_ != 3))

  it should "return true for other non-empty streams" in
    assert(Stream(1, 2, 3, 4, 5).forAll(x => x > 0 && x < 6))

  it should "be lazy" in {
    val s: Stream[Int] = Empty :+ 5 :+ sys.error("foo") :+ 3 :+ 2 :+ 1
    assert(!s.forAll(_ != 3))
  }

  "map" should "handle empty streams" in
    assert { Stream.empty[Int].map(_ + 1) == Empty }

  it should "handle non-empty streams" in
    assert { Stream(1, 2, 3).map( _ + 1).toList == List(2, 3, 4) }

  "filter" should "handle empty streams" in
    assert { Stream.empty[Int].map(_ % 2 == 0) == Empty }

  it should "handle non-empty streams" in
    assert { Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList == List(2, 4) }

  "append" should "handle empty streams" in
    assert { Stream.empty[Int].append(1).toList == List(1) }

  it should "handle non-empty streams" in
    assert { Stream(1, 2).append(3).toList == List(1, 2, 3) }

  it should "be lazy" in {
    val s = Stream(1, 2, 3).append(sys.error("foo"))
    assert(s.takeWhile(_ < 3).toList == List(1, 2))
  }

  "flatMap" should "handle empty streams" in
    assert { Stream.empty[Int].flatMap(i => Stream(i + 1)) == Empty }

  it should "handle non-empty streams" in
    assert {
      Stream(1, 2, 3).flatMap(i => Stream(i + 1)).toList == List(2, 3, 4)
      Stream(1, 2, 3).flatMap(i => if (i % 2 == 0) Empty else Stream(i + 1)).toList == List(2, 4)
    }

  "++" should "handle empty streams" in {
    assert { Stream.empty[Int] ++ Stream.empty[Int] == Stream.empty[Int] }
    assert { (Stream.empty[Int] ++ Stream(1, 2, 3)).toList == List(1, 2, 3) }
    assert { (Stream(1, 2, 3) ++ Stream.empty[Int]).toList == List(1, 2, 3) }
  }

  it should "handle non-empty streams" in
    assert { (Stream(1, 2, 3) ++ Stream(4, 5)).toList == List(1, 2, 3, 4, 5) }

  "ones" should "terminate" in
    assert { Stream.ones.take(3).toList == List(1, 1, 1) }

  "from" should "increment on demand" in
    assert { Stream.from(10).take(5).toList == List(10, 11, 12, 13, 14) }

  "fibs" should "generate the Fibonacci sequence" in
    assert { Stream.fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8) }

  "tail" should "handle empty streams" in
    assert { Stream.empty[Int].tail == Empty }

  it should "handle non-empty streams" in {
    assert { Stream(1).tail == Empty }
    assert { Stream(1, 2).tail.toList == List(2) }
    assert { Stream(1, 2, 3).tail.toList == List(2, 3) }
  }
}
