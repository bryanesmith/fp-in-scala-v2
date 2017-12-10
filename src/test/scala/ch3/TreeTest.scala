package ch3

import ch3.Tree._
import org.scalatest.FlatSpec

class TreeTest extends FlatSpec {

  "size" should "handle leaf" in
    assert(Leaf(1).size == 1)

  it should "handle branches" in
    assert((Leaf(1) :+ Leaf(2) :+ Leaf(3) :+ Leaf(4)).size == 7)

  "maximum" should "handle leaf" in
    assert(Leaf(-2).maximum == -2)

  it should "handle branches" in
    assert((Leaf(-3) :+ Leaf(1) :+ Leaf(2) :+ Leaf(-7)).maximum == 2)

  "depth" should "handle leaf" in
    assert(Leaf(-2).depth == 1)

  it should "handle branches" in {
    assert((Leaf(1) :+ Leaf(2)).depth == 2)
    assert((Leaf(1) :+ Leaf(2) :+ Leaf(3)).depth == 3)
    assert((Leaf(1) :+ Leaf(2) :+ Leaf(3) :+ Leaf(4)).depth == 4)
  }
}
