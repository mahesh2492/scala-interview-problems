package strings

import scala.annotation.tailrec
import scala.collection.mutable.Stack
object ParenthesisProblem extends App {

  /*
    "()" = true
    "()()" = true
    "(())" = true
    "))" = false
   */
  def hasValidParenthesis(string: String): Boolean = {
    @tailrec
    def validParenTailRec(remaining: String, openParen: Int): Boolean = {
      if(remaining.isEmpty) {
        openParen == 0
      }
      else if (openParen == 0 && remaining.head == ')') false
      else if (remaining.head == '(') validParenTailRec(remaining.tail, openParen + 1)
      else validParenTailRec(remaining.tail, openParen - 1)
    }
    validParenTailRec(string, 0)
  }

  def testValidParenthenses() = {
    println(hasValidParenthesis("()"))
    println(hasValidParenthesis(")("))
    println(hasValidParenthesis("()()"))
    println(hasValidParenthesis("(())"))
    println(hasValidParenthesis("())"))
    println(hasValidParenthesis(")()"))
  }

  testValidParenthenses()
}
