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
    def pascal(c: Int, r: Int): Int =
      c match {
        case 0 => 1
        case _ => if (c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceInner(chars: List[Char], acc: Int) : Boolean =
        chars match {
          case head :: tail => {
            val newAcc = head match {
              case '(' => acc + 1
              case ')' => acc - 1
              case _ => acc
            }
            if ( newAcc < 0 ) false
            else balanceInner(tail, newAcc)
          }
          case _ => acc == 0
        }
      balanceInner(chars,0)
    }

  /**
   * Exercise 3
   */
    //def countChange(money: Int, coins: List[Int]): Int = ???

    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeInner(money: Int, coins: List[Int]): Int = {
        if (money == 0)
          1
        else if (money < 0 || coins.isEmpty)
          0
        else
          countChangeInner(money,coins.tail) + countChangeInner(money - coins.head, coins)
      }
    countChangeInner(money,coins)
    }
}
