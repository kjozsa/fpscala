
object Chapter4_Option extends App {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(x) => Some(f(x))
      case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(x) => x
      case None => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(x) => f(x)
      case None => None
    }

    def flatMap2[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def orElse[B >: A](ob: => Option[B]): Option[B] = map(a => Some(a)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(x) if f(x) => Some(x)
      case _ => None
    }

    def filter2(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  object Option {
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      a.foldLeft(Some(List[B]()): Option[List[B]]) {
        case (Some(acc), elem) =>
          f(elem) match {
            case Some(a: A) => Some(acc ++ List[B](f(elem).getOrElse(sys.error("boo"))))
            case None => None
          }
        case (None, _) => None
      }
    }

//    def sequence[A](as: List[Option[A]]): Option[List[A]] =

  }

  val a1: Option[Int] = Some(3)
  val a2: Option[Int] = None

  println(a1.map(_ * 2))
  println(a2.map(_ * 2))
  println(a2 getOrElse 5)
  println(a1 filter (_ < 0))
  println(a1 filter (_ > 0))
  println(a2 filter (_ > 0))

  object others {
    def variance(xs: Seq[Double]): Option[Double] = {
      def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

      for {
        m <- mean(xs)
        v <- mean(xs.map(x => math.pow(x - m, 2)))
      } yield v
    }

    def map2[A, B, C](as: Option[A], bs: Option[B])(f: (A, B) => C): Option[C] =
      for {
        a <- as
        b <- bs
      } yield f(a, b)
  }

  println(Option.traverse(List(1,2,3,4))(x => if (x < 0) None else Some(x)))
  println(Option.traverse(List(1,2,3,4))(x => if (x < 2) None else Some(x)))

}
