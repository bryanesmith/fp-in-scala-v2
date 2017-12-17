package ch4

import org.scalatest.FlatSpec
import ch4.SeqOps._

class SeqOpsTest extends FlatSpec {

  val doubleNil:Seq[Double] = Nil

  "toNel" should "handle empty list" in
    assert(doubleNil.toNel == None)

  it should "handle non-empty list" in
    assert(Seq(1.0).toNel == Some(Seq(1.0)))

  "mean" should "handle empty list" in
    assert(doubleNil.mean == None)

  it should "handle non-empty list" in
    assert(Seq(1.0, 2.0, 3.0).mean == Some(2.0))

  "variance" should "handle empty list" in
    assert(doubleNil.variance == None)

  it should "handle non-empty list" in
    assert(Seq(1.0, 2.0, 3.0).variance == Some(2.0))
}
