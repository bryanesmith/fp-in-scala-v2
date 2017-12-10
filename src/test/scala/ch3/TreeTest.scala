package ch3

import ch3.Tree._
import org.scalatest.FlatSpec

class TreeTest extends FlatSpec {

  "size" should "handle leaf" in
    assert(Leaf(1).size == 1)

  it should "handle branches" in {
    val branch1 = Leaf(1) + Leaf(2)
    assert(branch1.size == 3)
    assert((branch1 + branch1).size == 7)
  }
}
