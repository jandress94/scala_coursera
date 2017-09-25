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
      if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balance_counter(chars: List[Char], numOutstandingOpens: Int):Boolean =
        if (chars.isEmpty) numOutstandingOpens == 0
        else {
          val nextChar = chars.head

          if (nextChar == '(')
            balance_counter(chars.tail, numOutstandingOpens + 1)
          else if (nextChar == ')' ) {
            if (numOutstandingOpens > 0) balance_counter(chars.tail, numOutstandingOpens - 1)
            else false
          }
          else
            balance_counter(chars.tail, numOutstandingOpens)
        }

      balance_counter(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def sumOptions(coins: List[Int], sum: Int, chosenNumOfCoin: Int, maxNumThisCoin: Int, denom: Int): Int = {
        if (chosenNumOfCoin > maxNumThisCoin) sum
        else sumOptions(coins, sum + countChange(money - chosenNumOfCoin * denom, coins), chosenNumOfCoin + 1, maxNumThisCoin, denom)
      }

      if (money == 0) {
        1
      } else if (coins.isEmpty) {
        0
      } else {
        val denom = coins.head
        val maxNumThisCoin = money / denom

        sumOptions(coins.tail, 0, 0, maxNumThisCoin, denom)
      }
    }
  }
