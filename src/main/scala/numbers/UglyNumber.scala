package numbers

import scala.annotation.tailrec

object UglyNumber extends App{

  /*
   ugly = only the factor 2, 3, and 5
   1 is ugly
   examples: 6, 25, 100
   not ugly: 14, 39
   */

  @tailrec
  def uglyNumber(number: Int): Boolean = {
    if(number == 1) true
    else if(number % 2 == 0) uglyNumber(number / 2)
    else if(number % 3 == 0) uglyNumber(number / 3)
    else if(number % 5 == 0) uglyNumber(number / 5)
    else false

  }

  // the nth ugly number, given the index
  // 1 is first ugly number
  def nthUgly(index: Int): Int =
    LazyList.from(1).filter(uglyNumber).apply(index)


  println(uglyNumber(6)) // true as 2 * 3
  println(uglyNumber(25)) // true as 5 * 5
  println(uglyNumber(100))
  println(uglyNumber(14))
  println(uglyNumber(39))

  println((0 to 99).toList.map(nthUgly))
}
