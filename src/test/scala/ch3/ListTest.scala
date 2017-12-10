package ch3

import ch3.List._
import org.scalatest.FlatSpec

class ListTest extends FlatSpec {

  "List.sum" should "handle empty list" in {
    assert(sum(Nil) == 0)
  }

  it should "handle non-empty list" in {
    assert(sum(List(1,2,3)) == 6)
  }

  "List.product" should "handle empty list" in {
    assert(product(Nil) == 1)
  }

  it should "handle non-empty list" in {
    assert(product(List(2.0, 2.0, 4.0)) == 16.0)
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
    assert(tail(Nil) == Nil)
  }

  it should "handle a list of one" in {
    assert(tail(List(1)) == Nil)
  }

  it should "handle a non-empty list" in {
    assert(tail(List(1, 2, 3)) == List(2, 3))
  }

  "List.setHead" should "handle an empty list" in {
    assert(setHead(1, Nil) == List(1))
  }

  it should "handle an non-empty list" in {
    assert(setHead(1, List(2, 3)) == List(1, 2, 3))
  }

  "List.drop" should "handle an empty list" in {
    assert(drop(Nil, 1) == Nil)
  }

  it should "handle negative n" in {
    assert(drop(List(1, 2, 3), -11) == List(1, 2, 3))
  }

  it should "handle n less than length of list" in {
    val l = List(1, 2, 3)
    assert(drop(l, 1) == List(2, 3))
    assert(drop(l, 2) == List(3))
  }

  it should "handle n equal to length of list" in {
    assert(drop(List(1, 2, 3), 3) == Nil)
  }

  it should "handle n greater length of list" in {
    assert(drop(List(1, 2, 3), 4) == Nil)
  }

  "List.dropWhile" should "handle an empty list" in {
    assert(dropWhile(Nil, (_: Int) => false) == Nil)
  }

  it should "handle a predicate that fails on first ele" in {
    assert(dropWhile(List(1, 2, 3), (_: Int) => false) == List(1, 2, 3))
  }

  it should "handle a predicate that succeeds on some ele" in {
    val l = List(1, 2, 3)
    assert(dropWhile(l, (a: Int) => a <= 1) == List(2, 3))
    assert(dropWhile(l, (a: Int) => a <= 2) == List(3))
  }

  it should "handle a predicate that succeeds on all ele" in {
    assert(dropWhile(List(1, 2, 3), (_: Int) => true) == Nil)
  }

  "List.init" should "handle an empty list" in {
    assert(init(Nil) == Nil)
  }

  it should "handle a non-empty list" in {
    assert(init(List(1)) == Nil)
    assert(init(List(1, 2)) == List(1))
    assert(init(List(1, 2, 3)) == List(1, 2))
  }

  // Exercise 3.8
  "List.tail" should "solve exercise 3.8" in {
    val found = foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
    assert(found == List(1, 2, 3))
  }
}
