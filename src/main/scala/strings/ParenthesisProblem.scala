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

  /*
     n = 1 => List("()")
     n = 2 => List("()()", "(())")
     n = 3 => List("()()()","()(())", "(())()" ,"((()))")
   */
  def generateAllValidParentheses(n: Int): List[String] = {
     @tailrec
     def genParensTailrec(nRemainingParens: Int, currentString: Set[String]): Set[String] = {
       if(nRemainingParens == 0) currentString
       else {
         val newStrings = for {
           string <- currentString
           index <- 0 until string.length
         } yield {
           val (before, after) = string.splitAt(index)
           s"$before()$after"
         }
         genParensTailrec(nRemainingParens - 1, newStrings)
       }
     }
    assert(n >= 0)
    if(n == 0) List()
    else genParensTailrec(n, Set("()")).toList

  }

  def testGenParens() = {
    println(generateAllValidParentheses(1))
    println(generateAllValidParentheses(2))
    println(generateAllValidParentheses(3))
    println(generateAllValidParentheses(10))
  }
  def testValidParenthenses() = {
    println(hasValidParenthesis("()"))
    println(hasValidParenthesis(")("))
    println(hasValidParenthesis("()()"))
    println(hasValidParenthesis("(())"))
    println(hasValidParenthesis("())"))
    println(hasValidParenthesis(")()"))
  }

  //testValidParenthenses()

  testGenParens()

}
