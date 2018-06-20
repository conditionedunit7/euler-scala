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
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(a,as) => as
  }

  def drop[A](as: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(as: List[A], acc: Int): List[A] = {
      if (acc == n) as
      else if (as == Nil) Nil
      else go(tail(as), acc+1)
    }
    
    if (n < 0) as
    else go(as, 0)
  }

  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
