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
      case _ => Stream(acc: _*) // Empty or predicate fails
    }
    go(this, Nil)
  }

  def contains[T >: A](t: T): Boolean = this.exists(_ == t)

  def exists(p: A => Boolean): Boolean = this.find(p).isDefined

  // lazy, but not tailrec.
  final def find(p: A => Boolean): Option[A] =
    this.foldRight(Option.empty[A])((a, b) => if (p(a)) Some(a) else b)

  // tailrec, but non-lazy.
//  @annotation.tailrec
//  final def find(p: A => Boolean): Option[A] = this match {
//    case Cons(h, _) if p(h()) => Some(h())
//    case Cons(_, t) => t().find(p)
//    case _ => None
//  }

  // lazy, but not tailrec.
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // tailrec, but non-lazy.
//  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
//    @annotation.tailrec
//    def go(s: Stream[A], acc: B): B = s match {
//      case Cons(h, t) => go(t(), f(h(), acc))
//      case _ => acc
//    }
//    go(this, z)
//  }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => if (!p(a)) false else b)
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

  // non-lazy constructor
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  implicit class StreamOps[A] (s: Stream[A]) {
    // unlike apply, this is a lazy variadic constructor
    def :+(a: => A): Stream[A] = cons(a, s)
  }

} // Stream


