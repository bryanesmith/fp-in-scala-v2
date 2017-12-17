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
    this.flatMap { (a) => if (f(a)) Some(a) else None }

  def toSeq: Seq[A] = this.map(a => Seq(a)).getOrElse(Nil)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def lift[A,B] (f: A => B): Option[A] => Option[B] = _ map f

  // Exercise 4.3
  def map2[A,B,C](aOpt: Option[A], bOpt:Option[B])(f: (A,B) => C): Option[C] =
    for {
      a <- aOpt
      b <- bOpt
    } yield f(a,b)

  // Exercise 4.4
  def sequence[A](a: Seq[Option[A]]): Option[Seq[A]] =
    traverse(a) { a => a }

  // Exercise 4.5
  def traverse[A,B](a: Seq[A])(fn: A => Option[B]): Option[Seq[B]] =
    Some(a.flatMap(a => fn(a).toSeq)).filter(_.length == a.length)
}
