package chapters



sealed trait Tree[+A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]


object Chapter3_Trees extends App {

  def size[A](xs: Tree[A]): Int = xs match {
    case Branch(left, right) => 1 + size(left) + size(right)
    case Leaf(_) => 1
  }

  def maximum(xs: Tree[Int]): Int = xs match {
    case Branch(left, right) => maximum(left) max maximum(right)
    case Leaf(value) => value
  }

  def depth[A](xs: Tree[A]): Int = xs match {
    case Branch(left, right) => 1 + (depth(left) max depth(right))
    case Leaf(_) => 0
  }

  def map[A, B](xs: Tree[A])(f: A => B): Tree[B] = xs match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(x) => Leaf(f(x))
  }

  def fold[A, B](xs: Tree[A])(f: A => B)(merge: (B, B) => B): B = xs match {
    case Branch(left, right) => merge(fold(left)(f)(merge), fold(right)(f)(merge))
    case Leaf(x) => f(x)
  }

  def size2[A](xs: Tree[A]): Int = fold(xs)(_ => 1)(_ + _)

  def maximum2(xs: Tree[Int]): Int = fold(xs)(x => x)(_ max _)

  def map2[A, B](xs: Tree[A])(f: A => B): Tree[B] = fold(xs)(x => Leaf(f(x)): Tree[B])(Branch(_, _))


  val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(5)), Leaf(4)))
  println(size(tree))
  println(maximum(tree))
  println(depth(tree))
  println(map(tree)(_ * 10))
  println(map2(tree)(_ * 10))


}
