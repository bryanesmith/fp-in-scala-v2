package ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  implicit class IntTreeOps (t: Tree[Int]) {
    // Exercise 3.26
    def maximum: Int = {
      def go(t1: Tree[Int]): Int = t1 match {
        case Branch(l, r) => go(l).max(go(r))
        case Leaf(a) => a
      }
      go(t)
    }
  }

  /**
    * So I can use infix methods on trees
    */
  implicit class TreeOps[A] (t: Tree[A]) {

    def +(other:Tree[A]): Tree[A] = Branch(t, other)

    // Exercise 3.25
    def size: Int = {
      def go(t1: Tree[A]): Int = t1 match {
        case Branch(l, r) => 1 + go(l) + go(r)
        case _ => 1
      }
      go(t)
    }

  } // TreeOps
}
