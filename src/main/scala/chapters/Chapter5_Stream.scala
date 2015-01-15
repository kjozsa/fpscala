package chapters

import scala.annotation.tailrec

object Chapter5_Stream extends App {

  sealed trait Stream[+A] {

    import chapters.Chapter5_Stream.Stream._

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(head, _) => Some(head())
    }

    def toList: List[A] = {
      @tailrec
      def loop(s: Stream[A], acc: List[A]): List[A] = s match {
        case Empty => acc
        case Cons(head, tail) => loop(tail(), head() :: acc)
      }

      loop(this, List())
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(head, tail) if n >= 1 => cons(head(), tail().take(n - 1))
      case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(head, tail) if n > 1 => tail().drop(n - 1)
      case Cons(head, tail) => tail()
      case _ => empty
    }

    def takeWhile(predicate: A => Boolean): Stream[A] = this match {
      case Cons(head, tail) if predicate(head()) => cons(head(), tail().takeWhile(predicate))
      case _ => empty
    }

    def forAll(predicate: A => Boolean): Boolean = this match {
      case Cons(head, tail) if !predicate(head()) => false
      case Cons(_, tail) => tail() forAll predicate
      case Empty => true
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def takeWhile2(predicate: A => Boolean): Stream[A] =
      foldRight(empty[A]) { (elem, acc) =>
        if (predicate(elem)) cons(elem, acc)
        else Empty
      }

    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((elem, acc) => cons(f(elem), acc))

    def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((elem, acc) => if (f(elem)) cons(elem, acc) else acc)

    def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((elem, acc) => cons(elem, acc))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((elem, acc) => f(elem) append acc)

    override def toString = this match {
      case Empty => ""
      case Cons(head, tail) => head().toString + ", " + tail().toString
    }

    def map2[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(head, tail) => Some((f(head()), tail()))
      case _ => None
    }

    def take2(n: Int): Stream[A] = unfold((this, n)) {
      case (Cons(head, tail), n) if n > 0 => Some((head(), (tail(), n - 1)))
      case _ => None
    }

    def takeWhile3(f: A => Boolean): Stream[A] = unfold(this) {
      case Cons(head, tail) if f(head()) => Some((head(), tail()))
      case _ => None
    }

    def zipWith[B](s: Stream[B]): Stream[(A, B)] = unfold((this, s)) {
      case (Cons(head, tail), Cons(head2, tail2)) => Some((head(), head2()), (tail(), tail2()))
      case _ => None
    }

    def startsWith[A](s: Stream[A]): Boolean = (this zipWith s) forAll { case ((a, b)) => a == b }

    def tails: Stream[Stream[A]] = unfold(this) {
      case Cons(head, tail) => Some(Cons(head, tail) -> tail())
      case _ => None
    }

    def exists(f: A => Boolean): Boolean = this match {
      case Cons(h, t) if f(h()) => true
      case Cons(h, t) => t().exists(f)
      case _ => false
    }

    def hasSubsequence[A](s: Stream[A]) = this.tails.exists(_.startsWith(s))
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def loop(i: Int, j: Int): Stream[Int] = {
        cons(i, loop(j, i + j))
      }

      loop(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((x, s)) => cons(x, unfold(s)(f))
      case None => empty
    }

    def fibs2: Stream[Int] = unfold((0, 1)) { case (a, b) => Some((a, (b, a + b)))}

    def from2(n: Int): Stream[Int] = unfold(n)(z => Some(z, z + 1))

    def constant2(n: Int): Stream[Int] = unfold(n)(_ => Some(n, n))

    def ones: Stream[Int] = unfold(1)(_ => Some(1, 1))

  }

  // cut here

  import chapters.Chapter5_Stream.Stream._

  def naturals: Stream[Int] = {
    def loop(next: Int): Stream[Int] = cons(next, loop(next + 1))
    loop(1)
  }

  println(naturals take 3)
  println((naturals take 10) drop 3)
  println(naturals takeWhile (_ < 5))
  println(naturals takeWhile (_ < 5) forAll (_ < 10))
  println(naturals takeWhile (_ < 5) forAll (_ < 2))
  println(naturals takeWhile2 (_ < 5))
  println((naturals map (_ * 3)) take 10)
  println(((naturals map (_ * 3)) take 10) filter (_ <= 15))
  println((naturals take 3) append (naturals take 4))
  println(from(3) take 3)
  println(from2(3) take 3)
  println(fibs take 8)
  println(fibs2 take 8)
  println(constant2(1) take 8)
  println(ones take 8)
  println((naturals map2 (_ * 3)) take 10)
  println((naturals map2 (_ * 3)) take2 10)
  println(naturals takeWhile3 (_ < 5))
  println(naturals.tails.take(5).map { z => z.take(3) })
  println(from(5).take(10) hasSubsequence from(8).take(5))
}
