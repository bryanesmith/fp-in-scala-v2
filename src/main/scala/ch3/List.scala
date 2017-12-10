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

  // Exercise 3.2
  def tail[A](as:List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, tail: List[A]) => tail
  }

  // Exercise 3.3
  def setHead[A](a:A, as:List[A]) = Cons(a, as)

}
