package ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * So I can use infix methods on Lists of Int
    */
  implicit class IntListOps[A <: Int] (l: List[A]) {

    private val empty = List[Int]()

    def sum: Int = {
      def go(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x:Int, xs:List[Int]) => x + go(xs)
      }
      go(l)
    }

    // Exercise 3.11
    def sum2: Int = l.foldLeft(0) { _ + _ }

    // Exercise 3.16
    def addOne: List[Int] = {
      @annotation.tailrec
      def sub(is:List[Int], acc:List[Int]): List[Int] = is match {
        case Cons(head, tail) => sub(tail, acc.append(List(head+1)))
        case Nil => acc
      }
      sub(l, empty)
    }

  } // IntListOps

  /**
    * So I can use infix methods on Lists of Double
    */
  implicit class DoubleListOps[A <: Double] (l: List[A]) {

    def product(): Double = {
      def go(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x: Double, xs: List[Double]) => x * go(xs);
      }
      go(l)
    }

    // Exercise 3.11
    def product2(): Double = l.foldLeft(1.0) { _ * _ }

  } // DoubleListOps

  /**
    * So I can use infix methods on Lists of Lists
    */
  implicit class ListListOps[A] (l: List[List[A]]) {

    private val empty = List[A]()

    // Exercise 3.15 - O(n log n)
//    def flatten: List[A] = l.foldLeft(empty) { (acc: List[A], next:List[A]) => acc.append(next) }

    // Exercise 3.15 - O(2n) but tailrec
//    def flatten: List[A] =  l.foldLeft(empty) {
//      (acc: List[A], next:List[A]) => next.foldLeft(acc) { (z, a) => Cons(a, z) }
//    }.reverse

    // Exercise 3.15 - ~O(n) but eats stack
    def flatten: List[A] = {
      def go(ls:List[List[A]]): List[A] = ls match {
        case Cons(head: List[A], tail: List[List[A]]) => head.append(go(tail))
        case Nil => Nil
      }
      go(l)
    }
  }

  /**
    * So I can use infix methods on any type of List
    */
  implicit class ListOps[A] (l: List[A]) {

    private val empty = List[A]()

    def foldRightEatsStack[B](z: B)(f: (A, B) => B): B = {
      def go(as: List[A]): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, go(xs))
      }
      go(l)
    }

    def append(a2: List[A]): List[A] = {
      def go(a1: List[A]): List[A] = a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, go(t))
      }
      go(l)
    }

    // Exercise 3.2
    def tail: List[A] = drop(1)

    // Exercise 3.3
    def setHead(a:A) = Cons(a, l)

    // Exercise 3.4
    def drop(num: Int): List[A] = {
      @annotation.tailrec
      def go(as: List[A], n: Int): List[A] = as match {
        case Nil => Nil
        case Cons(_, tail:List[A]) if n > 0 => go(tail, n - 1)
        case _ => as
      }
      go(l, num)
    }

    // Exercise 3.5
    def dropWhile(f: A => Boolean): List[A] = {
      @annotation.tailrec
      def go(as: List[A]): List[A] = as match {
        case Nil => Nil
        case Cons(head, tail:List[A]) if f(head) => go(tail)
        case _ => as
      }
      go(l)
    }

    // Exercise 3.6
    def init: List[A] = {
      def go(as: List[A]): List[A] = as match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(head, tail:List[A]) => Cons(head, go(tail))
      }
      go(l)
    }

    // Exercise 3.9
    def lengthRight: Int = foldRight(0) { (_, b) => b + 1 }

    // Exercise 3.10
    def foldLeft[B](z:B)(f: (B, A) => B): B = {
      @annotation.tailrec
      def go(as:List[A], acc: B): B = as match {
        case Nil => acc
        case Cons(x, xs) => go(xs, f(acc, x))
      }
      go(l, z)
    }

    // Exercise 3.11
    def length: Int = foldLeft(0) { (b, _) => b + 1 }

    // Exercise 3.12
    def reverse: List[A] = foldLeft(empty) { (acc, a) => Cons(a, acc) }

    // Exercise 3.13
    def reverseRight: List[A] = foldRight(empty) { (a, acc) => acc.append(List(a)) }

    // Exercise 3.13
    def foldLeftBad[B](z:B)(f: (B, A) => B): B =
      this.reverseRight.foldRightEatsStack(z)((a: A, b: B) => f(b, a))

    // Exercise 3.13
    def foldRight[B](z:B)(f: (A, B) => B): B =
      this.reverse.foldLeft(z)((b: B, a: A) => f(a, b))

    // Exercise 3.14
    def append2(a2: List[A]): List[A] =
      a2.foldRight(l) { (a, acc) => Cons(a, acc)}

  } // ListOps

} // object List

