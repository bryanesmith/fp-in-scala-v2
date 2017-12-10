package ch3

import org.scalatest.FlatSpec

class ListTest extends FlatSpec {

  "List.sum" should "handle empty list" in {
    assert(List.sum(Nil) == 0)
  }

  it should "handle non-empty list" in {
    assert(List.sum(List(1,2,3)) == 6)
  }

  "List.product" should "handle empty list" in {
    assert(List.product(Nil) == 1)
  }

  it should "handle non-empty list" in {
    assert(List.product(List(2.0, 2.0, 4.0)) == 16.0)
  }

  // exercise 3.1
  "List" should "solve exercise 3.1" in {
    val found = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    assert(found == 3)
  }

  "List.tail" should "handle an empty list" in {
    assert(List.tail(Nil) == Nil)
  }

  it should "handle a list of one" in {
    assert(List.tail(List(1)) == Nil)
  }

  it should "handle a non-empty list" in {
    assert(List.tail(List(1, 2, 3)) == List(2, 3))
  }

  "List.setHead" should "handle an empty list" in {
    assert(List.setHead(1, Nil) == List(1))
  }

  it should "handle an non-empty list" in {
    assert(List.setHead(1, List(2, 3)) == List(1, 2, 3))
  }


}
