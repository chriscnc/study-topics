sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x,xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_,xs) if(n > 0) => drop(xs, n-1)
    case _ => l
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
  l match {
    case Cons(h,t) if f(h) => dropWhile(t)(f)
    case _ => l 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = {
    def loop[A](l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(x, Nil) => acc
      case Cons(x,xs) => loop(xs, append(acc, List(x)))
    }
    loop(l, Nil)
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    l match {
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    l match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs, f(x,z))(f)
    }

  def sum2(l: List[Int]): Int = 
    foldRight(l, 0)((x,y) => x + y)

  def sum3(l: List[Int]): Int = 
    foldLeft(l, 0)((x,y) => x + y)

  def product2(l: List[Double]): Double =
    foldRight(l, 1.0)((x,y) => x * y)

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)((x,y) => x * y)

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((_,acc) => acc + 1)

  def length2[A](l: List[A]): Int = 
    foldLeft(l, 0)((_,acc) => acc + 1)

  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, Nil:List[A])((h,acc) => Cons(h, acc))

  def add1(l: List[Int]): List[Int] = 
    foldRight(l, Nil:List[Int])((h,a) => Cons(h+1, a))

  def doubleToString(l: List[Double]): List[String] = 
    foldRight(l, Nil:List[String])((h,a) => Cons(h.toString, a))

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, Nil:List[B])((h,a) => Cons(f(h), a))

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,a) => if(p(h)) Cons(h,a) else a)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = 
    foldRight(l, Nil:List[B])((h,a) => append(f(h), a)) 

  def filterViaFlatMap[A](l: List[A])(p: A => Boolean): List[A] = 
    flatMap(l)(a => if(p(a)) List(a) else Nil)

  def zipIntList(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, zipIntList(t1, t2))
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  val t = Branch(Leaf(1), Branch(Branch(Leaf(2),Leaf(4)), Leaf(3)))

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + depth(l) max depth(r) 
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f)) 
  }

  // def fold[A,B](t: Tree[A], z: B)(f: (A,B) => B): B = t match {
// 
  // } 

}



