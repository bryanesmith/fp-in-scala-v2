package ch6

object Rand {

  type Rand[A] = State.StateType[RNG, A]

  // Exercise 6.9
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a => (f(a), _) }

  // Exercise 6.6, 6.9
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(rb) { b =>
      flatMap(ra) { a =>
        (f(a, b), _)
      }
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb) { (_,_) }

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @annotation.tailrec
    def go(current: RNG, rem: List[Rand[A]], acc: List[A]): (List[A], RNG) = rem match {
      case Nil => (acc, current)
      case rnd +: tail =>
        val (a, next) = rnd(current)
        go(next, tail, acc :+ a)
    }
    rng => go(rng, fs, Nil)
  }

  // Exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => f(rng) match {
      case (a, rng2) => g(a)(rng2)
    }
}
