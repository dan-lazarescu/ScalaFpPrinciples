package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c < 0 || r < 0) 0
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
  
    def count(chars: List[Char], acc: Int): Int =
  
      if (chars.isEmpty || acc < 0) acc
      else if (chars.head == '(') count(chars.tail, acc + 1)
      else if (chars.head == ')') count(chars.tail, acc - 1)
      else count(chars.tail, acc)
  
    if (count(chars, 0) == 0) true
    else false

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    val new_coins = coins.sorted

    def change(money: Int, coins: List[Int]): Int =
      if (money == 0) 1
      else if (!coins.isEmpty && (money - coins.head >= 0))
        change(money - coins.head, coins) + change(money, coins.tail)
      else 0

    if (money == 0) 0
    else change(money, new_coins)
