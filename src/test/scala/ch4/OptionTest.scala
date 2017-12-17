package ch4

import org.scalatest.FlatSpec

class OptionTest extends FlatSpec {

  val intNone:Option[Int] = None

  "map" should "handle Some" in
    assert(Some(4).map(_ + 1) == Some(5))

  it should "handle None" in
    assert(intNone.map(_ + 1) == None)

  "flatMap" should "handle Some" in
    assert(Some(4).flatMap(i => Some(i + 1)) == Some(5))

  "flatMap" should "handle Nones" in {
    assert(intNone.flatMap(i => Some(i + 1)) == None)
    assert(Some(4).flatMap(_ => None) == None)
  }

  "getOrElse" should "handle Some" in
    assert(Some(4).getOrElse(5) == 4)

  it should "handle None" in
    assert(None.getOrElse(5) == 5)

  "orElse" should "handle Some" in
    assert(Some(4).orElse(Some(5)) == Some(4))

  it should "handle None" in
    assert(intNone.orElse(Some(5)) == Some(5))

  "filter" should "handle Some" in {
    assert(Some(4).filter(_ > 3) == Some(4))
    assert(Some(4).filter(_ <= 3) == None)
  }

  it should "handle None" in
    assert(intNone.filter(_ > 3) == None)

}
