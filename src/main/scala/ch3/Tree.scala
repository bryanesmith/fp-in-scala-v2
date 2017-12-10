package ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

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
