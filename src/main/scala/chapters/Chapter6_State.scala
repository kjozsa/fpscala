package chapters

object Chapter6_State extends App {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object RNG {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (math.signum(i) * i -> r)
    }

    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      (i / (1 + Int.MaxValue.toDouble) -> r)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r) = rng.nextInt
      val (d, r2) = double(r)
      ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (d, r1) = double(rng)
      val (i, r2) = r1.nextInt
      ((d, i), r2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r2)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      count match {
        case 0 => (List(), rng)
        case n => {
          val (i, r) = rng.nextInt
          val (is, r2) = ints(n - 1)(r)
          (i :: is, r2)
        }
      }
    }

    // with a better API

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = (a, _)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (s2, r2) = s(rng)
      (f(s2), r2)
    }

    def double2: Rand[Double] = map(_.nextInt)(_ / Int.MaxValue.toDouble)

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs.foldLeft((List[A](), rng)) { (acc, elem) =>
      val (a, r) = elem(acc._2)
      (a :: acc._1) -> r
    }
  }


  // cut here
  import RNG._
  println((1 to 4).toList map (i => nonNegativeInt(SimpleRNG(i))))
  println((1 to 4).toList map (i => double(SimpleRNG(i))))
  println((1 to 4).toList map (i => intDouble(SimpleRNG(i))))
  println(ints(5)(SimpleRNG(1)))

  val ss: Rand[List[Int]] = sequence(List(nonNegativeInt, nonNegativeInt, nonNegativeInt))
  println(ss(SimpleRNG(1)))
}
