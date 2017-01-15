package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c:Int,r:Int): Int = {

    def factorial(n: Int): Int = {

      def loop(acc: Int, n: Int): Int =
        if (n == 0) acc
        else loop(acc * n, n - 1)

      loop(1, n)
    }

    def abs(x:Int):Int = if (x<0) -x else x

    if (r==c ||  c <= 0)
      1
    else
      factorial(r) / (factorial(c) * factorial(abs(r - c)))
  }
  
  /**
   * Exercise 2
   */
  def balance(chars:List[Char]):Boolean = {
    def score (chars:List[Char],sum : Int): Int= {
      if (chars.isEmpty) sum
      else if (chars.head.toString == "(" ) {
        score(chars.tail,sum+1)
      }
      else if (chars.head.toString == ")" && sum ==0 ) {
        score(chars.tail,sum-2)
      }
      else if (chars.head.toString == ")" && sum != 0) {
        score(chars.tail,sum-1)
      }
      else
        score(chars.tail,sum)

    }

    if (score(chars,0) == 0) true
    else false
  }

  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0) //no money
      1
    else if (money > 0 && !coins.isEmpty)
      countChange(money - coins.head,coins) + countChange(money,coins.tail)
    else
      0
    }
}

