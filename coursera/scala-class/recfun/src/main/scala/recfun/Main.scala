package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = {
    if(r < 2) 1
    else 
      if(c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanced(nesting: Int, chars: List[Char]): Boolean = {
      if(chars.isEmpty) nesting == 0
      else 
        if(chars.head == '(') balanced(nesting + 1, chars.tail)
        else if (chars.head == ')')
          if(nesting > 0) balanced(nesting - 1, chars.tail)
          else false 
        else balanced(nesting, chars.tail) 
    }
    balanced(0, chars)
  } 

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 0 
    else if(coins.isEmpty) 0 
    else if(coins.head > money) countChange(money, coins.tail)
    else if(coins.head == money) 1 + countChange(money, coins.tail)
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
