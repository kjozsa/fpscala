package chapters


object Chapter2 extends App {

  // == Excercise 2.1
  def fib(n: Int): Int = {
    def fibo(x: Int, y: Int, count: Int): Int = {
      if (count == 0) x
      else fibo(y, x + y, count - 1)
    }

    fibo(0, 1, n)
  }

  //  Range(0, 10).foreach(x => println(x + " -> " + fib(x)))


  // == Excercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.size < 2) true
    else ordered(as(0), as(1)) && isSorted(as.drop(1), ordered)
  }

  //  assert (isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x < y))
  //  assert (!isSorted(Array(1, 2, 2, 3, 4, 5), (x: Int, y: Int) => x < y))


  // == Excercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { a: A => b: B =>
    f(a, b)
  }

  val f1: Int => (Int => Int) = curry((a: Int, b: Int) => a + b)


  // == Excercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a: A, b: B) =>
    f(a)(b)
  }

  val f2: (Int, Int) => Int = uncurry((a: Int) => (b: Int) => a + b)


  // == Excercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = { a: A =>
    f(g(a))
  }

  val f3: (Int => String) = compose((a: String) => a.toUpperCase, (b: Int) => b.toString)

}
