package ch6

object Rand {

  type Rand[A] = State[RNG, A]

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    ra.map2(rb) { (_,_) }

}
