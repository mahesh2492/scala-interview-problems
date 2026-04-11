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

  def testCountCharacter() = {
    println(countCharacters("Scala"))
    println(countCharacters("I love Scala and Functional Programming language because it is awesome."))
  }

  /*
     "silent" = "listen"
     "brag" = "grab"
   */
  def checkAnagrams(sa: String, sb: String): Boolean = {
     val saList = sa.toList
     sb.forall(saList.contains(_))
  }

  def checkAnagramsv2(sa: String, sb: String): Boolean = countCharacters(sa) == countCharacters(sb)
  def checkAnagramsv3(sa: String, sb: String): Boolean = sa.sorted == sb.sorted


  def testCheckAnagrams() = {
    println(checkAnagrams("silent", "listen"))
    println(checkAnagrams("scala", "haskel"))
    println(checkAnagramsv2("brag", "grab"))
    println(checkAnagramsv2("scala", "haskel"))
    println(checkAnagramsv3("brag", "grab"))
  }
  def main(args: Array[String]): Unit = {
    //testCountCharacter()
    testCheckAnagrams()
  }
}
