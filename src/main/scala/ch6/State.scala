package ch6

case class State[S, +A](run: S => (A, S)) {

  /**
    * For fetching state in for comprehension:
    *
    *   for {
    *     s <- this.get
    *   }
    */
  def get: State[S,S] = State { s => (s,s) }

  def set(s: S): State[S, Unit] = State { _ => ((), s) }

  /**
    * Modifies state using provided function.
    */
  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  // Exercise 6.9, Exercise 6.10
  def map[B](f: A => B): State[S, B] =
    this.flatMap { a =>
      State { (f(a), _) }
    }

  // Exercise 6.6, 6.9, 6.10
  def map2[B,C](rb: State[S, B])(f: (A,B) => C): State[S, C] =
    rb.flatMap { b =>
      this.flatMap { a =>
        State { (f(a, b), _) }
      }
    }

  // Exercise 6.8, 6.10
  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s =>
      run(s) match {
        case (a, s2) => g(a).run(s2)
      }
    )

}

object State {

  // Exercise 6.10
  def unit[S,A](a: A): State[S,A] = State { (s:S) => (a, s) }

  // Exercise 6.7, 6.10
  def sequence[A,S](fs: List[State[S,A]]): State[S,List[A]] = {
    @annotation.tailrec
    def go(current: S, rem: List[State[S,A]], acc: List[A]): (List[A], S) = rem match {
      case Nil => (acc, current)
      case rnd +: tail =>
        val (a, next) = rnd.run(current)
        go(next, tail, acc :+ a)
    }
    State { go(_, fs, Nil) }
  }
}
