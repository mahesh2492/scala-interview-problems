package numbers

import scala.annotation.tailrec

object ParseInteger extends App {

  /*
     Return a number from the string argument:
     - there may be leading spaces, ignore those
     - read the sign character if present
     - read all the digits until the end of string or until the non-digit character
     - return the number formed from those digits
     - if the number exceeds the int range, return either Int.MinValue (underflow) or Int.MaxFlow (overflow)

   */
  def parseInteger(string: String): Int = {
    val WHITESPACE = ' '
    val PLUS = '+'
    val MINUS = '-'
    val DIGITS = "0123456789".toSet

    def integerRangeEnd(sign: Int): Int = if (sign >= 0) Int.MaxValue else Int.MinValue

    @tailrec
    def parseTailrec(remainder: String, sign: Int, acc: Int = 0): Int = {
      if (remainder.isEmpty || !DIGITS.contains(remainder.charAt(0))) acc
      else {
        val newDigit = remainder.charAt(0) - '0'
        val tentativeResult = acc * 10 + newDigit * sign
        if ((sign >= 0) != (tentativeResult >= 0)) integerRangeEnd(sign)
        else parseTailrec(remainder.substring(1), sign, tentativeResult)
      }
    }

    if(string.isEmpty) 0
    else if(string.charAt(0) == WHITESPACE)
      parseInteger(string.substring(1))
    else if (string.charAt(0) == PLUS)
      parseTailrec(string.substring(1), sign = 1)
    else if (string.charAt(0) == MINUS)
      parseTailrec(string.substring(1), sign = -1)
    else
      parseTailrec(string, sign = 1)

  }

  println(parseInteger("")) // 0
  println(parseInteger("String")) // 0
  println(parseInteger("1234"))
  println(parseInteger("-1234"))
  println(parseInteger("  7658"))
  println(parseInteger("  +17658"))
  println(parseInteger("   Scala"))
  println(parseInteger("42 is the meaning of life")) // 42
  println(parseInteger("     42 is the meaning of life")) // 42
  println(parseInteger(Int.MaxValue.toString)) // Int.MaxValue
  println(parseInteger(Int.MinValue.toString)) // Int.MinValue
}
