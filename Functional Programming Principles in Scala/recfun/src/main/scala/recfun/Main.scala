package recfun

import scala.collection.immutable.Stack

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))

    println(countChange(4, List(1, 2)))
    println(countChange(8, List(1, 2, 4)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) {
    1
  } else {
    pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def iterate(chars: List[Char], stack: List[Char]): Boolean = chars match {
      case Nil => stack.isEmpty
      case head :: tail => head match {
        case '(' => iterate(tail, head :: stack)
        case ')' => stack match {
          case Nil => false
          case _ :: bottom => iterate(tail, bottom)
        }
        case _ => iterate(tail, stack)
      }
    }

    iterate(chars, List())
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = money match {
    case 0 => 1
    case x if x < 0 => 0
    case x if x > 0 => coins.zipWithIndex.map(
      coinAndIndex => countChange(money - coinAndIndex._1, coins.slice(coinAndIndex._2, coins.length))
    ).sum
  }
}
