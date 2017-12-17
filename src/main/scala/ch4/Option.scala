package ch4

sealed trait Option[+A] {

  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  // Exercise 4.1
  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  // Exercise 4.1
  def getOrElse[B >: A](ob: => B): B = this match {
    case Some(a) => a
    case None => ob
  }

  // Exercise 4.1
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(a => Some(a)).getOrElse(ob)

  // Exercise 4.1
  def filter(f: A => Boolean): Option[A] =
    this.flatMap ((a) => if (f(a)) { Some(a) } else { None })
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


