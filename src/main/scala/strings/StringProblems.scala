package strings

import scala.annotation.tailrec

object StringProblems {

  //scala -> s -> 1, c -> 1, a -> 2, l -> 1
  def countCharacters(s: String): Map[Char, Int] = {
    @tailrec
    def countCharTailrec(remaining: String, acc: Map[Char, Int]) : Map[Char, Int] = {
      if(remaining.isEmpty) {
        acc
      } else if(acc.contains(remaining.head)) {
        val updatedFrequency: Int = acc(remaining.head) + 1
        countCharTailrec(remaining.tail, acc + (remaining.head -> updatedFrequency))
      } else {
        countCharTailrec(remaining.tail,  acc + (remaining.head -> 1))
      }
    }

    countCharTailrec(s.trim, Map.empty[Char, Int])
  }

  def main(args: Array[String]): Unit = {
    println(countCharacters("Scala"))
    println(countCharacters("I love Scala and Functional Programming language because it is awesome."))
  }
}
