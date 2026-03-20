package list

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  /**
   * standard functions
   * @return
   */
  def head:T
  def tail: RList[T]
  def isEmpty: Boolean
  //Changed prepend name to :: to make it right associative
  // RNil.::(2) == 2 :: RNil
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T

  def length: Int
  //reverse the list
  def reverse: RList[T]

  //concatenate another list to this one
  def ++[S >: T](anotherList: RList[S]): RList[S]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException()

  override def length: Int = 0

  override def reverse: RList[Nothing] = RNil

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList
}

//Renamed Cons to :: as scala original collection
case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
     def toStringTailRec(remaining: RList[T], result: String): String = {
       if(remaining.isEmpty) result
       else if(remaining.tail.isEmpty) s"$result${remaining.head}"
       else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")
     }
    "["  + toStringTailRec(this, "") + "]"
  }

  /**
   * Easy problems
   */
  // get element at a given index
  override def apply(index: Int): T = {
     @tailrec
     def applyTailRec(remaining: RList[T], currentIndex: Int): T = {
        if(index == currentIndex) remaining.head
        else applyTailRec(remaining.tail, currentIndex + 1)
     }

     if(index < 0) throw new NoSuchElementException
     else applyTailRec(this, 0)
  }

  //size of the list
  override def length: Int = {
      @tailrec
      def lengthTailRec(remaining: RList[T], len: Int): Int = {
        remaining match {
          case RNil => len
          case ::(_, tail) => lengthTailRec(tail, len + 1)
        }
      }

    lengthTailRec(this, 0)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseTailRec(remainingList: RList[T], result: RList[T]): RList[T] = {
      remainingList match {
        case RNil => result
        case ::(head, tail) => reverseTailRec(tail, head :: result)
      }
    }
    reverseTailRec(this, RNil)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def append(remainingList: RList[S], result: RList[S]): RList[S] = {
        if(remainingList.isEmpty) {
          result
        } else {
          append(remainingList.tail, remainingList.head :: result)
        }
    }
    append(anotherList, this.reverse).reverse
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def convertToRListTailRec(remaining: Iterable[T], acc: RList[T]): RList[T] = {
        if(remaining.isEmpty) acc
        else convertToRListTailRec(remaining.tail, remaining.head :: acc)
    }
    convertToRListTailRec(iterable, RNil).reverse
  }
}
object ListProblem extends App {
  val aSmallList =  1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  val aLargeList = RList.from(1 to 10000)
  //test l-th
  println(aSmallList.apply(1))
  println(aSmallList.apply(2))
  println(aLargeList.apply(8345))

  //test length
  println(aSmallList.length)
  println(aLargeList.length)

  //test reverse
  println(aSmallList.reverse)
  println(aLargeList.reverse)

  val anotherSmallList = 4 :: 5 :: 6 :: RNil

  println(aSmallList ++ anotherSmallList)

}
