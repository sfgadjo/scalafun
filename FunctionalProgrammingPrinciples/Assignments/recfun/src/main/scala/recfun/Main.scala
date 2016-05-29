package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parentheses Balancing")
    balance("(if (zero? x) max (/ 1 x))".toList)

    println("Counting Change")
    countChange(4, List(1,2))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if ((c <= 0 || r <= 0) || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def go(l: List[Char], numOpen: Int, numClosed: Int): Boolean = {
      l match {
        case Nil if numOpen == 0 && numClosed == 0 => true
        case ')' :: _ if numOpen <= 0 => false
        case '(' :: t => go(t, numOpen + 1, numClosed + 1)
        case ')' :: t => go(t, numOpen - 1, numClosed - 1)
        case _ :: t => go(t, numOpen, numClosed)
        case _ => false
      }
    }
    go(chars, 0, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def go(m: Int, l: List[Int]): Int = {
      if(m == 0) 1
      else if(m < 0 || l.isEmpty) 0
      else{
        val h = l.head
        val t = l.tail
        go(m, t) + go(m - h, l)
      }
    }
    go(money, coins)
    }
  }
