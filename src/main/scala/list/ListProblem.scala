package list

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head:T
  def tail: RList[T]
  def isEmpty: Boolean
  //Changed prepend name to :: to make it right associative
  // RNil.::(2) == 2 :: RNil
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
  def apply(index: Int): T
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException()
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

  override def apply(index: Int): T = {
     @tailrec
     def applyTailRec(remaining: RList[T], currentIndex: Int): T = {
        if(index == currentIndex) remaining.head
        else applyTailRec(remaining.tail, currentIndex + 1)
     }

     if(index < 0) throw new NoSuchElementException
     else applyTailRec(this, 0)
  }
}

object ListProblem extends App {
  val aSmallList =  1 :: 2 :: 3 :: 4 :: 5 :: RNil // RNil.::(5).::(4).::(3).::(2).::(1)
  println(aSmallList(3))
}
