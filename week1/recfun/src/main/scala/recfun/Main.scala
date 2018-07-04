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
    def pascal(c: Int, r: Int): Int = {
      if (r == 0) 1
      else if (c == 0 || c == r) 1
      else pascal(c , r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val l = '('
      val r = ')'

      def balance_helper(chars: List[Char], stack: List[Char]): Boolean = {
        if (chars.isEmpty) stack.isEmpty
        else {
          if (chars.head == l) balance_helper(chars.tail, l::stack)
          else if (chars.head == r)  {
            if (stack.isEmpty) false
            else if (stack.head == l) balance_helper(chars.tail, stack.tail)
            else balance_helper(chars.tail, r::stack)
          }
          else balance_helper(chars.tail, stack)
        }
      }

      balance_helper(chars, List())
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money ==  0) 1
      else if (money < 0 || coins.isEmpty) 0
      else {
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
    }
  }
