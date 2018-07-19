sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def foldRight[A,B](as: List[A], default: B)(f: (A, B) => B): B = 
    as match {
      case Nil => default
      case Cons(x,xs) => f(x, foldRight(xs, default)(f))
    }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x,y) => 1 + y)

  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
