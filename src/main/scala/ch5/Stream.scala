package ch5

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // Exercise 5.1
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), acc :+ h()) // Warning: append is O(n)
    }

    go(this, Nil)
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], r: Int, acc: List[A]): Stream[A] = s match {
      case Cons(h, t) if r > 0 => go(t(), r - 1, acc :+ h()) // Warning: append is O(n)
      case _ => Stream(acc: _*)
    }
    go(this, n, Nil)
  }

  // Exercise 5.2
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ if n == 0 => this
    case _ => Empty  // Empty or n < 0
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): Stream[A] = s match {
      case Cons(h, t) if p(h()) => go(t(), acc :+ h()) // Warning: append is O(n)
      case _ => Stream(acc: _*)
    }
    go(this, Nil)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)
  }

  // for type inference
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
