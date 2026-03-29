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
}
