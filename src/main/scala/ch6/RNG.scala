package ch6

trait RNG {
  def nextInt: (Int, RNG)
}
