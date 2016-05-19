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
  def pascal(c: Int, r: Int): Int = 
	  if (c == 0 || c == r) 1
  	  else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
	def count(chars1: List[Char], c: Int) : Boolean =
      if (c < 0) false
	  else if (chars1.isEmpty) true
      else if (chars1.head == '(') count(chars1.tail, c+1)
	  else if (chars1.head == ')') count(chars1.tail, c-1)
	  else count(chars1.tail, c)
	  
	count(chars, 0)
  }	

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

	  def count(money: Int, coins: List[Int]) : Int = 
		  if (money == 0) 1
	  	  else if (coins.isEmpty) 0
		  else if (money < 0) 0
		  else count(money-coins.head, coins) + count(money, coins.tail)

	  count(money, coins)
  }
}
