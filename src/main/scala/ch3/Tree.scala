package ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  implicit class IntTreeOps (t: Tree[Int]) {
    // Exercise 3.26
    def maximum: Int = t.fold(Integer.MIN_VALUE) {(a,b) => a max b }
  }

  /**
    * So I can use infix methods on trees
    */
  implicit class TreeOps[A] (t: Tree[A]) {

    def :+(other:Tree[A]): Tree[A] = Branch(t, other)

    // Exercise 3.29
    def fold[B](z: B)(f: (A, B) => B): B =
      t.traverse(z)((t1, b) => t1 match {
        case Leaf(a) => f(a, b)
        case _ => b
      })

    // Exercise 3.29
    def traverse[B](z: B)(f: (Tree[A], B) => B): B = {
      def go(t1: Tree[A], acc: B): B = t1 match {
        case Leaf(_) => f(t1, acc)
        case Branch(l, r) => go(r, go(l, f(t1, acc)))
      }
      go(t, z)
    }

    // Exercise 3.25
    def size: Int = t.traverse(0) {(_, b) => b + 1}

    // Exercise 3.27
    def depth: Int = {
      def go(t1: Tree[A]): Int = t1 match {
        case Branch(l, r) => 1 + go(l).max(go(r))
        case _ => 1
      }
      go(t)
    }

    // Exercise 3.28
    def map[B](f: (A) => B): Tree[B] = {
      def go(t1: Tree[A]): Tree[B] = t1 match {
        case Branch(l, r) => Branch(go(l), go(r))
        case Leaf(a) => Leaf(f(a))
      }
      go(t)
    }

  } // TreeOps
}
