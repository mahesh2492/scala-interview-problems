package numbers

import scala.annotation.tailrec

object NumberProblems extends App {

  def isPrime(n: Int): Boolean = {
    n > 1 && !(2 to math.sqrt(n).toInt).exists(i => n % i == 0)
  }

  //Complexiety - O(sqrt(N))
  def isPrimeV2(n: Int): Boolean = {
    @tailrec
    def isPrimeTailrec(currentDivisor: Int): Boolean = {
      if(currentDivisor > Math.sqrt(n)) true
      else n % currentDivisor != 0 && isPrimeTailrec(currentDivisor + 1)
    }
    if(n == 0 || n == 1) false
    else isPrimeV2(2)
  }


  /*
     the constituent prime divisor
     4 = 2 * 2
     6 = 2 * 3
     10 = 2 * 5
     12 = 3 * 4 , 1, 2, 3, 4, 6, 12
     Complexity - O(Sqrt(n))
   */
  def decompose(n: Int): List[Int] = {
    assert(n > 0)
    @tailrec
    def decomposeTailRec(remaining: Int, currentDivisor: Int, acc: List[Int]): List[Int] = {
      if(currentDivisor > Math.sqrt(remaining)) remaining :: acc
      else if(remaining % currentDivisor == 0) decomposeTailRec(remaining / currentDivisor, currentDivisor, currentDivisor :: acc)
      else decomposeTailRec(remaining, currentDivisor + 1, acc)
    }

    decomposeTailRec(n, 2, Nil)
  }

  println(decompose(16))
  println(decompose(27))
}
