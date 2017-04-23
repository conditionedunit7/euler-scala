def fib(n: Int): Int = {
  @annotation.tailrec
  def go(m: Int, first: Int, second: Int): Int = 
    if (m == n)
      first + second
    else
      go (m+1, second, first + second)

  if (n < 1)
    -1
  else if (n == 1)
    0
  else if (n == 2)
    1
  else
    go (3, 0, 1)
}
