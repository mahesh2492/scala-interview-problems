package numbers

object Duplicate {

  // all numbers in the list appear EXACTLY twice, EXCEPT one: find that number
  def duplicate(list: List[Int]): Int = {
    list.reduceLeft(_ ^ _)
  }

  def main(args: Array[String]): Unit = {
      println(duplicate(List(1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6, 7, 7)))
  }
}
