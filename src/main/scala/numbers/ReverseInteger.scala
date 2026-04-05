package numbers

import scala.annotation.tailrec

object ReverseInteger extends App {

  //return number with digits reversed
  // if the result overflows Int, return 0
  def reverseInteger(n: Int): Int = {
    @tailrec
    def reverseTailRec(remaining: Int, acc: Int): Int = {
      if(remaining == 0) acc
      else {
        val remainder = remaining % 10

        val tentativeNumber = acc * 10 + remainder
        if((acc >= 0) != (tentativeNumber >= 0)) 0
        else reverseTailRec(remaining / 10, tentativeNumber)
      }
    }

    if(n == Int.MinValue) 0
    else if(n >= 0) reverseTailRec(n, 0)
    else -reverseTailRec(-n, 0)
  }

  println(reverseInteger(12))
  println(reverseInteger(534))
  println(reverseInteger(540))
  println(reverseInteger(-56787654))
  println(reverseInteger(Int.MaxValue))
}
