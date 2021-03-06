package ch3

import org.scalatest.FlatSpec

class ListTest extends FlatSpec {

  private val emptyInt = List[Int]()

  "sum" should "handle empty list" in
    assert(Nil.sum == 0)

  it should "handle non-empty list" in
    assert(List(1,2,3).sum == 6)

  "product" should "handle empty list" in
    assert(Nil.product == 1)

  it should "handle non-empty list" in
    assert(List(2.0, 2.0, 4.0).product == 16.0)

  // exercise 3.1
  "List" should "solve exercise 3.1" in {
    val found = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + t.sum
      case _ => 101
    }
    assert(found == 3)
  }

  "tail" should "handle an empty list" in
    assert(Nil.tail == Nil)

  it should "handle a list of one" in
    assert(List(1).tail == Nil)

  it should "handle a non-empty list" in
    assert(List(1, 2, 3).tail == List(2, 3))

  "setHead" should "handle an empty list" in
    assert(Nil.setHead(1) == List(1))

  it should "handle an non-empty list" in
    assert(List(2, 3).setHead(1) == List(1, 2, 3))

  "drop" should "handle an empty list" in
    assert(Nil.drop(1) == Nil)

  it should "handle negative n" in
    assert(List(1, 2, 3).drop(-11) == List(1, 2, 3))

  it should "handle n less than length of list" in {
    val l = List(1, 2, 3)
    assert(l.drop(1) == List(2, 3))
    assert(l.drop(2) == List(3))
  }

  it should "handle n equal to length of list" in
    assert(List(1, 2, 3).drop(3) == Nil)

  it should "handle n greater length of list" in
    assert(List(1, 2, 3).drop(4) == Nil)

  "dropWhile" should "handle an empty list" in
    assert(Nil.dropWhile { _: Int => false } == Nil)

  it should "handle a predicate that fails on first ele" in
    assert(List(1, 2, 3).dropWhile { _ => false } == List(1, 2, 3))

  it should "handle a predicate that succeeds on some ele" in {
    val l = List(1, 2, 3)
    assert(l.dropWhile { _ <= 1 } == List(2, 3))
    assert(l.dropWhile { _ <= 2 } == List(3))
  }

  it should "handle a predicate that succeeds on all ele" in
    assert(List(1, 2, 3).dropWhile { _ => true } == Nil)

  "init" should "handle an empty list" in {
    assert(Nil.init == Nil)
    assert(Nil.init.init == Nil)
  }

  it should "handle a non-empty list" in {
    assert(List(1).init == Nil)
    assert(List(1, 2).init == List(1))
    assert(List(1, 2, 3).init == List(1, 2))
  }

  // Exercise 3.8
  "tail" should "solve exercise 3.8" in {
    val found = List(1,2,3).foldRight(emptyInt) { Cons(_,_) }
    assert(List(1, 2, 3) == found )
  }

  "lengthRight" should "handle empty list" in
    assert(Nil.lengthRight == 0)

  it should "handle non-empty lists" in {
    assert(List(1).lengthRight == 1)
    assert(List(1, 2).lengthRight == 2)
    assert(List(1, 2, 3).lengthRight == 3)
  }

  "foldLeft" should "be add things" in {
    assert(emptyInt.foldLeft(0)(_ + _) == 0)
    assert(List(1).foldLeft(0)(_ + _) == 1)
    assert(List(1, 2).foldLeft(0)(_ + _) == 3)
  }

  it should "concatenate strings forwards" in {
    assert(List[String]().foldLeft("")(_ + _) == "")
    assert(List("a").foldLeft("")(_ + _) == "a")
    assert(List("a", "b").foldLeft("")(_ + _) == "ab")
    assert(List("a", "b", "c").foldLeft("")(_ + _) == "abc")
  }

  "sum2" should "handle empty list" in
    assert(Nil.sum2 == 0)

  it should "handle non-empty lists" in {
    assert(List(1).sum2 == 1)
    assert(List(1, 2).sum2 == 3)
  }

  "product2" should "handle empty list" in
    assert(Nil.product2 == 1.0)

  it should "handle non-empty list" in {
    assert(List(2.0).product2 == 2.0)
    assert(List(2.0, 3.0).product2 == 6.0)
  }

  "length" should "handle empty list" in
    assert(Nil.length == 0)

  it should "handle non-empty lists" in {
    assert(List(1).length == 1)
    assert(List(1, 2).length == 2)
    assert(List(1, 2, 3).length == 3)
  }

  "reverse" should "handle empty lists" in
    assert(Nil.reverse == Nil)

  it should "handle non-empty lists" in {
    assert(List(1).reverse == List(1))
    assert(List(1, 2).reverse == List(2, 1))
    assert(List(1, 2, 3).reverse == List(3, 2, 1))
  }

  "reverseRight" should "handle empty lists" in
    assert(Nil.reverseRight == Nil)

  it should "handle non-empty lists" in {
    assert(List(1).reverseRight == List(1))
    assert(List(1, 2).reverseRight == List(2, 1))
    assert(List(1, 2, 3).reverseRight == List(3, 2, 1))
  }

  "foldLeftBad" should "be add things" in {
    assert(emptyInt.foldLeftBad(0)(_ + _) == 0)
    assert(List(1).foldLeftBad(0)(_ + _) == 1)
    assert(List(1, 2).foldLeftBad(0)(_ + _) == 3)
  }

  it should "concatenate strings forwards" in {
    assert(List[String]().foldLeftBad("")(_ + _) == "")
    assert(List("a").foldLeftBad("")(_ + _) == "a")
    assert(List("a", "b").foldLeftBad("")(_ + _) == "ab")
    assert(List("a", "b", "c").foldLeftBad("")(_ + _) == "abc")
  }

  "append2" should "handle empty lists" in {
    assert(Nil.append(List(1, 2, 3)) == List(1, 2, 3))
    assert(List(1, 2, 3).append(Nil) == List(1, 2, 3))
    assert(Nil.append(Nil) == Nil)
  }

  it should "handle non-empty lists" in
    assert(List(1, 2).append(List(3, 4, 5)) == List(1, 2, 3, 4, 5))

  "flatten" should "handle empty lists" in {
    assert(Nil.flatten == Nil)
    assert(List(Nil, Nil, Nil).flatten == Nil)
  }

  it should "handle non-empty lists" in
    assert(List(List(1, 2), List(3, 4), List(5)).flatten == List(1, 2, 3, 4, 5))

  "plusOne" should "handle empty lists" in
    assert(Nil.plusOne == Nil)

  it should "handle non-empty lists" in
    assert(List(1, 2, 3).plusOne == List(2, 3, 4))

  "toStrings" should "handle empty lists" in
    assert(Nil.toStrings == Nil)

  it should "handle non-empty lists" in
    assert(List(1.0, 2.0, 3.0).toStrings == List("1.0", "2.0", "3.0"))

  "filter" should "handle empty lists" in
    assert(emptyInt.filter(_ => true) == Nil)

  it should "filter out odd numbers"
    assert(List(1, 2, 3, 4, 5).filter(_ % 2 == 0) == List(2, 4))

  "flatMap" should "handle empty lists" in {
    assert(emptyInt.flatMap(a => List(a, a)) == Nil)
    assert(List(1, 2, 3).flatMap(_ => Nil) == Nil)
  }

  it should "flatMap ftw" in
    assert(List(1, 2, 3).flatMap(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))

  "add" should "handle empty lists" in {
    assert(emptyInt.add(emptyInt) == emptyInt)
    assert(emptyInt.add(List(1,2,3)) == emptyInt)
    assert(List(1,2,3).add(emptyInt) == emptyInt)
  }

  it should "handle non-empty lists of equal length" in
    assert(List(1, 2, 3).add(List(10, 10, 10)) == List(11, 12, 13))

  it should "handle non-empty lists of unequal lengths" in {
    assert(List(1, 2, 3, 4).add(List(10, 10, 10)) == List(11, 12, 13))
    assert(List(1, 2, 3).add(List(10, 10, 10, 10)) == List(11, 12, 13))
  }

  "zip" should "handle empty lists" in {
    val empty = List[(Int,Int)]()
    assert(emptyInt.zip(emptyInt) == empty)
    assert(List(1, 2).zip(emptyInt) == empty)
    assert(emptyInt.zip(List(1, 2)) == empty)
  }

  it should "handle non-empty lists of equal length" in
    assert(List(1, 2).zip(List(11, 12)) == List((1, 11), (2, 12)))

  it should "handle non-empty lists of unequal lengths" in {
    assert(List(1, 2, 3).zip(List(11, 12)) == List((1, 11), (2, 12)))
    assert(List(1, 2).zip(List(11, 12, 13)) == List((1, 11), (2, 12)))
  }

  "forall" should "handle empty lists" in
    assert(emptyInt.forall(_ => false))

  it should "handle non-empty lists" in {
    assert(List(1, 2, 3, 4, 5).forall(_ > 0))
    assert(!List(1, 2, 3, 4, 5).forall(_ > 2))
  }

  "startWith" should "handle empty lists" in {
    assert(Nil.startWith(Nil))
    assert(List(1, 2, 3).startWith(Nil))
    assert(!Nil.startWith(List(1, 2, 3)))
  }

  it should "handle non-empty lists of less-than or equal length" in {
    assert(List(1, 2, 3).startWith(List(1)))
    assert(List(1, 2, 3).startWith(List(1, 2)))
    assert(List(1, 2, 3).startWith(List(1, 2, 3)))
    assert(!List(1, 2, 3).startWith(List(2, 3)))
  }

  it should "handle non-empty lists of greater size" in
    assert(!List(1, 2, 3).startWith(List(1, 2, 3, 4)))

  "hasSubsequence" should "handle empty lists" in {
    assert(Nil.hasSubsequence(Nil))
    assert(List(1, 2, 3).hasSubsequence(Nil))
    assert(!Nil.hasSubsequence(List(1, 2, 3)))
  }

  it should "handle non-empty lists of less-than or equal length" in {
    assert(List(1, 2, 3).hasSubsequence(List(1)))
    assert(List(1, 2, 3).hasSubsequence(List(2)))
    assert(List(1, 2, 3).hasSubsequence(List(3)))
    assert(List(1, 2, 3).hasSubsequence(List(1, 2)))
    assert(List(1, 2, 3).hasSubsequence(List(2, 3)))
    assert(List(1, 2, 3).hasSubsequence(List(1, 2, 3)))
    assert(!List(1, 2, 3).hasSubsequence(List(4)))
    assert(!List(1, 2, 3).hasSubsequence(List(3, 4)))
    assert(!List(1, 2, 3).hasSubsequence(List(2, 1)))
  }

  it should "handle non-empty lists of greater length" in
    assert(!List(1, 2, 3).hasSubsequence(List(1, 2, 3, 4)))
}
