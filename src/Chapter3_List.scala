

object Chapter3_List extends App {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def head[A](xs: List[A]): A = xs match {
      case Cons(x, xs) => x
      case _ => sys.error("headless")
    }

    def tail[A](xs: List[A]): List[A] = xs match {
      case Cons(_, xs) => xs
      case _ => Nil
    }

    def setHead[A](head: A, xs: List[A]): List[A] = xs match {
      case Cons(_, xs) => Cons(head, xs)
      case _ => Nil
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else drop(tail(l), n - 1)
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
      case _ => l
    }

    def init[A](l: List[A]): List[A] = l match {
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _)

    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_, acc) => acc + 1)
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def sumL(xs: List[Int]) = foldLeft(xs, 0)(_ + _)

    def productL(xs: List[Int]) = foldLeft(xs, 0)(_ * _)

    def lengthL(xs: List[Int]) = foldLeft(xs, 0)((acc, _) => acc + 1)

    def reverse[A](xs: List[A]) = foldLeft(xs, List[A]())((acc, elem) => Cons(elem, acc))

    def append[A](xs: List[A], xs2: List[A]) = foldRight(xs, xs2)((a, b) => Cons(a, b))

    def concat[A](xss: List[List[A]]) = foldRight(xss, List[A]())((a: List[A], b: List[A]) => append(a, b))

    def plusOne(xs: List[Int]): List[Int] = foldRight(xs, List[Int]())((elem, acc) => Cons(elem + 1, acc))

    def intToString(xs: List[Int]): List[String] = foldRight(xs, List[String]())((elem, acc) => Cons(elem.toString, acc))

    def map[A, B](xs: List[A])(f: A => B): List[B] = foldRight(xs, List[B]())((elem, acc) => Cons(f(elem), acc))

    def filter[A](xs: List[A])(f: A => Boolean): List[A] = foldRight(xs, List[A]())((elem, acc) => if (f(elem)) Cons(elem, acc) else acc)

    def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = concat(map(xs)(f))

    def fmFilter[A](xs: List[A], f: A => Boolean): List[A] = flatMap(xs)(x => if (f(x)) List(x) else Nil)

    //    def addLists[Int](xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    //      case (Nil, _) => Nil
    //      case (_, Nil) => Nil
    //      case (Cons(x, x1), Cons(y, y1)) => Cons(x + y, addLists(x1, y1))
    //    }

    def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

  }

  // == Excercise 3.1

  import Chapter3_List.List._

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  assert(x == 3)

  val xs: List[Int] = List(1, 2, 3, 4, 5)
  println(setHead(100, xs))
  println(drop(xs, 3))
  println(dropWhile(xs, (x: Int) => x <= 4))
  println(init(xs))
  println(length(xs))
  println(append(xs, List(6, 7, 8)))
  println(plusOne(xs))
  println(intToString(xs))
  println(map(xs)((x: Int) => x + 10))
  println(filter(xs)(_ % 2 == 0))
  println(flatMap(List(1, 2, 3))(i => List(i, i)))
  println(zipWith(xs, xs)(_ * _))

}
