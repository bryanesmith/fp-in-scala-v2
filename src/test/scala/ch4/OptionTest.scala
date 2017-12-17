package ch4

import org.scalatest.FlatSpec
import ch4.Option._

class OptionTest extends FlatSpec {

  val intNone:Option[Int] = None
  val optIntNil:Seq[Option[Int]] = Nil

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
    assert(Some(4).orElse(None) == Some(4))

  it should "handle None" in {
    assert(intNone.orElse(Some(5)) == Some(5))
    assert(intNone.orElse(None) == None)
  }

  "filter" should "handle Some" in {
    assert(Some(4).filter(_ > 3) == Some(4))
    assert(Some(4).filter(_ <= 3) == None)
  }

  it should "handle None" in
    assert { intNone.filter(_ > 3) == None }

  "map2" should "handle Some" in
    assert { map2(Some(1), Some(2))(_ + _) == Some(3) }

  it should "handle None" in {
    assert { map2(Some(1), None)(_ + _) == None }
    assert { map2(intNone, Some(2))(_ + _) == None }
  }

  "sequence" should "handle empty list" in
    assert { sequence(optIntNil) == Some(Nil) }

  it should "handle non-empty list of all Some" in {
    assert { sequence(Seq(Some(1.0))) == Some(Seq(1.0)) }
    assert { sequence(Seq(Some(1.0), Some(2.0))) == Some(Seq(1.0, 2.0)) }
  }

  it should "handle non-empty list containing None" in {
    assert { sequence(Seq(intNone)) == None }
    assert { sequence(Seq(Some(1.0), None)) == None }
    assert { sequence(Seq(intNone, Some(2.0))) == None }
  }
}
