package ch4

object SeqOps {

  implicit class DoubleSeqOps(xs: Seq[Double]) {

    // Exercise 4.2
    lazy val toNel: Option[Seq[Double]] = xs match {
      case Nil => None
      case a => Some(a)
    }

    // Exercise 4.2
    lazy val mean: Option[Double] =
      xs.toNel.map(xs => xs.foldLeft(0.0)(_ + _) / xs.length)

    // Exercise 4.2
    lazy val variance: Option[Double] =
      mean map { m => xs.map(a => Math.pow(a - m, 2)).sum }

  }
}
