package ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x:Int, xs:List[Int]) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x: Double, xs: List[Double]) => x * product(xs);
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  implicit class ListOps[A] (l: List[A]) {

    def foldRight[B](z: B)(f: (A, B) => B): B = {
      def go(as: List[A], z: B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, go(xs, z))
      }
      go(l, z)
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
        case Cons(_:A, tail:List[A]) if n > 0 => go(tail, n - 1)
        case _ => as
      }
      go(l, num)
    }

    // Exercise 3.5
    def dropWhile(f: A => Boolean): List[A] = {
      @annotation.tailrec
      def go(as: List[A]): List[A] = as match {
        case Nil => Nil
        case Cons(head:A, tail:List[A]) if f(head) => go(tail)
        case _ => as
      }
      go(l)
    }

    // Exercise 3.6
    def init: List[A] = {
      def go(as: List[A]): List[A] = as match {
        case Nil => Nil
        case Cons(a:A, Nil) => Nil
        case Cons(head:A, tail:List[A]) => Cons(head, go(tail))
      }
      go(l)
    }

    // Exercise 3.9
    def length: Int = foldRight(0) { (_, b) => b + 1 }

  } // ListOps

} // object List

