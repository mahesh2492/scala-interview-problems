val number = List(123, 45, 678)
val sortedList = number.sorted
val (first, second) = sortedList.span(_ < 10)

number.sorted
  .map(_.toString)
  .sortWith( (a, b) => a + b > b + a)
  .mkString
