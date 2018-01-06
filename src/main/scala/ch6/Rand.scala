package ch6

object Rand {
  type Rand[+A] = RNG => (A, RNG)
}
