package ch5

sealed trait Stream[+A] {

//  def headOption: Option[A] = this match {
//    case Empty => None
//    case Cons(h, _) => Some(h())
//  }

  // Exercise 5.6
  def headOption: Option[A] = this.foldRight(Option.empty[A]) { (a, _) => Some(a) }

  def tail: Stream[A] = this match {
    case Cons(_, t) => t()
    case Empty => Empty
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
//  def take(n: Int): Stream[A] = {
//    @annotation.tailrec
//    def go(s: Stream[A], r: Int, acc: List[A]): Stream[A] = s match {
//      case Cons(h, t) if r > 0 => go(t(), r - 1, acc :+ h()) // Warning: append is O(n)
//      case _ => Stream(acc: _*)
//    }
//    go(this, n, Nil)
//  }

  // Exercise 5.13
  def take(n: Int): Stream[A] =
    Stream.unfold(this) { s =>
      if (n > 0) s.headOption.map { a => (a, s.tail.take(n - 1)) }
      else None
    }

  // Exercise 5.2
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ if n == 0 => this
    case _ => Empty  // Empty or n < 0
  }

  // Exercise 5.3
//  def takeWhile(p: A => Boolean): Stream[A] = {
//    @annotation.tailrec
//    def go(s: Stream[A], acc: List[A]): Stream[A] = s match {
//      case Cons(h, t) if p(h()) => go(t(), acc :+ h()) // Warning: append is O(n)
//      case _ => Stream(acc: _*) // Empty or predicate fails
//    }
//    go(this, Nil)
//  }

  // Exercise 5.5
//  def takeWhile(p: A => Boolean): Stream[A] =
//    this.foldRight(Stream.empty[A]) { (a,b) => if (p(a)) Stream.cons(a, b.takeWhile(p)) else Empty }

  // Exercise 5.13
  def takeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this) (s => s.headOption match {
      case Some(a) if p(a) => Some((a, s.tail.takeWhile(p)))
      case _ => None
    })

  def contains[T >: A](t: T): Boolean = this.exists(_ == t)

  def exists(p: A => Boolean): Boolean = this.find(p).isDefined

  // lazy, but not tailrec.
  final def find(p: A => Boolean): Option[A] = this.filter(p).headOption

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
    this.foldRight(true) { (a, b) => if (!p(a)) false else b }

  // Exercise 5.7
//  def map[B](f: A => B): Stream[B] =
//    this.foldRight(Stream.empty[B]) { (a, b) => Stream.cons(f(a), b) }

  // Exercise 5.13
  def map[B](f: A => B): Stream[B] =
    Stream.unfold(this)(s => s.headOption.map { a => (f(a), s.tail) })

  // Exercise 5.7
  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A]) { (a, b) =>
      if (f(a)) Stream.cons(a, b.filter(f)) else b.filter(f)
    }

  // Exercise 5.7
  def append[T >: A](a: => T): Stream[T] =
     this ++ Stream.cons(a, Empty)

  // Exercise 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Stream.empty[B]) ((a, b) => f(a) match {
      case Empty => b
      case s => s ++ b
    })

  def ++[T >: A](other: Stream[T]): Stream[T] =
    this.foldRight(other) { (a, b) => Stream.cons(a, b) }

  // Exercise 5.13
  def zipWith[B >: A,C](other: Stream[B])(f: (B,B) => C): Stream[C] =
    Stream.unfold((this, other)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // Exercise 5.13
  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, other)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), Empty))
      case (_, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  /**
    * Like zipAll, drains the other stream; but unlike zipAll, won't drain this stream.
    */
  def zipRight[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, other)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (_, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  // Exercise 5.14
  def startsWith[T >: A](other: Stream[T]): Boolean = this.zipRight(other).forAll {
    case ((Some(t), Some(o))) => t == o
    case ((Some(_), _)) => true
    case _ => false
  }

  // Exercise 5.15
  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Cons(h, t) => Some((Cons(h, t), t()))
    case _ => None
  }

  // Exercise 5.16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    this.tails.map(s => s.foldRight(z)(f)).append(z)

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

  // Exercise 5.8, 5.12
  def constant[A](a: A): Stream[A] =
    Stream.unfold(a) { s => Some((s, s))}

  val ones: Stream[Int] = constant(1)

  // Exercise 5.9, 5.12
  def from(n: Int): Stream[Int] =
    Stream.unfold(n) { i => Some((i, i + 1)) }

  // Exercise 5.10, 5.12
  def fibs: Stream[Int] =
    Stream.unfold((0, 1)) { case((l, r)) => Some((l, (r, l + r))) }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Empty
  }

  implicit class StreamOps[A] (s: Stream[A]) {
    // unlike apply, this is a lazy variadic constructor
    def :+(a: => A): Stream[A] = cons(a, s)
  }

} // Stream


