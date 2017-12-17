package ch4

import org.scalatest.FlatSpec

class EitherTest extends FlatSpec {

  // To help Either type inference
  private def ++(i: Int) = i + 1

  // To help Either type inference
  private def ++!(i: Int) = Right(i + 1)

  // To help Either type inference
  private def add(i1: Int, i2: Int) = i1 + i2

  "map" should "handle right" in
    assert { Right(2).map(++) == Right(3) }

  it should "handle left" in
    assert { Left("abc").map(++) == Left("abc") }

  "flatMap" should "handle right" in
    assert { Right(2).flatMap(++!) == Right(3) }

  it should "handle left" in {
    assert { Left("abc").flatMap(++!) == Left("abc") }
    assert { Right(2).flatMap(_ => Left("abc")) == Left("abc") }
  }

  "orElse" should "handle right" in {
    assert { Right(2).orElse(Left("abc")) == Right(2) }
    assert { Right(2).orElse(Right(3)) == Right(2) }
  }

  it should "handle left" in {
    assert { Left("abc").orElse(Right(2)) == Right(2) }
    assert { Left("abc").orElse(Left("def")) == Left("def") }
  }

  "map2" should "handle right" in
    assert { Right(3).map2(Right(2))(_ + _) == Right(5) }

  it should "handle left" in {
    assert { Left("abc").map2(Right(2))(add) == Left("abc") }
    assert { Right(2).map2(Left("abc"))(add) == Left("abc") }
    assert { Left("abc").map2(Left("def"))(add) == Left("abc") }
  }
}
