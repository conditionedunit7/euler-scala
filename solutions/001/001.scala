// Project Euler Problem 1
object eulerModule001 {
  // Return the sum of all numbers below n that are a multiple of 3 or 5
  def problem(n: Int): Int = 
  {
    @annotation.tailrec
    def go(i: Int, acc: Int): Int = 
    {
      if (i >= n) acc
      else go(i+1, acc+multipleOfThreeOrFive(i))
    }

    go(0, 0)
  }

  def multipleOfThreeOrFive(x: Int): Int = 
  {
    if ((x % 3 == 0) || (x % 5 == 0)) x
    else 0
  }

  def main(args: Array[String]): Unit = 
    println(problem(1000))
}

