package chapters


object Chapter4_Either extends App {

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case a@Left(_) => a
      case Right(x) => Right(f(x))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case a@Left(_) => a
      case Right(x) => f(x)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case a => a
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case a :: as => for {
        aa <- f(a)
        bb <- traverse(as)(f)
      } yield (aa :: bb)
    }

    def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = traverse(as)(identity)
  }

}
