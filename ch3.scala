sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  //provided
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  // end provided

  def apply[A](le: A*): List[A] = le match {
    case le if (le.nonEmpty) => Cons(le.head, apply(le.tail: _*))
    case _ => Nil
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => throw new Exception("Can't take the tail of an empty list")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case _ => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Cons(_, t) => drop(t, n-1)
      case Nil => Nil
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _ => l

  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => Nil
  }

  def length[A](l: List[A]): Int = {
    List.foldRight(l, 0)((_, count) => 1+count)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sum(l: List[Int]): Int = foldLeft(l, 0)((x, y) => x+y)

  def product(ld: List[Double]): Double = foldLeft(ld, 1.0)(_*_)

  def lengthFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((x,_) => x+1)

  // Had to look at example for this one...
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((newTail, newHead) => Cons(newHead, newTail))

  def append[A](l: List[A], appended: A): List[A] = foldRight(l, Cons(appended, Nil))(Cons(_, _))

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Cons(h, t) => Cons(f(h), map(t)(f))
    case Nil => Nil
  }

  def flatMap[A](l: List[A]): List[A] = ???

}




sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = {
    def sizeInner[A](it: Tree[A], acc: Int): Int = it match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + sizeInner(l, acc) + sizeInner(r, acc)
    }
    sizeInner(t, 0)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + depth(l).max(depth(r))  // Why is `1 + depth(l) max depth(r)` a different result?
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(n) => f(n)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maxFold(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depthFold[A](t: Tree[A]): Int = 1 + fold(t)(_ => 1)(1 + _ max _)


}






object Main extends App {
  val li = List(1,2,3,4,5)
  val ld = List(1.0, 2.0, 3.0, 4.0, 5.0, 20.0)
  val ls = List("a", "bb", "ccc", "dddd", "eeeee")
  val lol = List(li, ld, ls)
  println("LISTS_____________")
  println(List.apply(1,2,3))
  println(List.tail(List(1,2,3)))
  println(List.setHead(li, 100))
  println(List.drop(li, 2))
  println(List.dropWhile(li, (x:Int) => x<3))
  println(List.init(li))
  println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  println(List.length(li))
  println(List.foldLeft(li, 0)((x, y) => x+y))
  println(List.sum(li))
  println(List.product(ld))
  println(List.lengthFoldLeft(ld))
  println(List.reverse(li))
  println(List.append(li, 20))
  println(List.map(li)(_+1))




  val t =
    Branch(
      Branch(
        Leaf(1000),
        Leaf(10)
      ),
      Branch(
        Branch(
          Branch(
            Branch(
              Branch(
                Leaf(20),
                Leaf(30)
              ),
              Leaf(40)
            ),
            Leaf(2)
          ),
          Leaf(3)
        ),
        Leaf(3)
      )
    )

  println("Trees__________")
  println(t)
  println(Tree.size(t))
  println(Tree.maximum(t))
  println(Tree.depth(t))
  println(Tree.map(t)(_*2))
  println(Tree.fold(t)(_*2)(_+_))
  println(Tree.sizeFold(t))
  println(Tree.maxFold(t))
  println(Tree.depthFold(t))
}
