package numbers

object LargesNumber extends App {

  /*
    Given a list of non-negative integers, arrange them such that they form the largest number.
    The result might be huge so return string.
    List(10, 2) = "210"
    List(3, 30, 5, 9, 34) = "9534330"
    sort - 2, 10
    sort - 3,5,9,30,34 -> 9533430
   */
  def largesNumber(numbers: List[Int]): String =  {
    if(numbers.isEmpty || numbers.forall(_ == 0)) "0"
    else numbers.sorted
     .map(_.toString)
     .sortWith( (a, b) => a + b > b + a)
     .mkString
  }

  val list1 = List(10, 2)
  val list2 = List(3, 30, 5, 9, 34)

  println(largesNumber(list1))
  println(largesNumber(list2))
  println(largesNumber(List(2020, 20, 1010, 10, 2, 22)))
  println(largesNumber(List(1)))
  println(largesNumber(List()))
  println(largesNumber(List(0, 0, 0, 0)))
}
