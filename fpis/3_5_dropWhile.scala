sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def go(as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(a, as) =>
        if (f(a)) go(as)
        else Cons(a,as)
    }
    
    go(l)
  }

  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
