package ch4

sealed trait Either[+E, +A] {

  // Exercise 4.6
  def map[B](f: A => B): Either[E, B] =
    flatMap(b => Right(f(b)))

  // Exercise 4.6
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(b) => f(b)
      case Left(a) => Left(a)
    }

  // Exercise 4.6
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(_) => this
    case _ => b
  }

  // Exercise 4.6
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
    for {
      a <- this
      b <- b
    } yield f(a, b)

  def isRight: Boolean = this match {
    case Right(_) => true
    case _ => false
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  // Exercise 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es) { a => a }

  // Exercise 4.7
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.map(f).partition(_.isRight) match {
      case (_, ls) if ls.nonEmpty =>
        Left(
          ls.head.asInstanceOf[Left[E]] match {
            case Left(e) => e
          }
        )
      case (rs, _) =>
        Right(
          rs.asInstanceOf[List[Right[B]]] map {
            case Right(a) => a
          }
        )
    }


}