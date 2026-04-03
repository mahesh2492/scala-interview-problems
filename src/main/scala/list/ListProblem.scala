package list

import scala.annotation.tailrec
import scala.util.Random

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

  // remove an element at a given index and return a new list
  def removeAt(index: Int): RList[T]

  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  /**
   *
     Medium problems
   */
  //run-length encoding
  def rle: RList[(T, Int)]

  // duplicate each element a number of times in a row
  def duplicateEach(k: Int):RList[T]

  // rotation by a number of position to the left
  def rotate(k: Int): RList[T]

  // random sample
  def sample(k: Int): RList[T]

  //sorting the list in the order defined by Ordering object
  def sorted[S >: T](ordering: Ordering[S]): RList[S]
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

  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  /**
   *
   *  Medium problems
   */
  override def rle: RList[(Nothing, Int)] = RNil

  override def duplicateEach(k: Int): RList[Nothing] = RNil

  override def rotate(k: Int): RList[Nothing] = RNil

  override def sample(k: Int): RList[Nothing] = RNil

  override def sorted[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
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

  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeTailRec(remainingList: RList[T], currentIndex: Int, acc: RList[T]): RList[T] = {
      remainingList match {
        case RNil => acc.reverse
        case ::(head, tail) =>
          if(currentIndex == index) {
            acc.reverse ++ tail
          } else {
            removeTailRec(tail, currentIndex + 1, head :: acc)
          }
      }
    }
    removeTailRec(this, 0, RNil)
  }

  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapTailRec(remainingList: RList[T], acc: RList[S]): RList[S] = {
      remainingList match {
        case RNil => acc
        case ::(head, tail) => mapTailRec(tail, f(head) :: acc)
      }
    }
    mapTailRec(this, RNil).reverse
  }


  // Complexity - O(N * N)
  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapTailRec(remainingList: RList[T], acc: RList[S]): RList[S] = {
      remainingList match {
        case RNil => acc
        case ::(head, tail) => flatMapTailRec(tail, acc ++ f(head))
      }
    }

    @tailrec
    def betterFlatMap(remaining: RList[T], accumulator: RList[RList[S]]): RList[S] = {
      if(remaining.isEmpty) concatenateAll(accumulator, RNil, RNil)
      else betterFlatMap(remaining.tail, f(remaining.head) :: accumulator)
    }

    @tailrec
    def concatenateAll(elements: RList[RList[S]], currentList: RList[S], accumulator: RList[S]): RList[S] = {
      if(currentList.isEmpty && elements.isEmpty) accumulator
      else if(currentList.isEmpty) concatenateAll(elements.tail, elements.head, accumulator)
      else concatenateAll(elements, currentList.tail, currentList.head :: accumulator)
    }

    flatMapTailRec(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterTailRec(remainingList: RList[T], acc: RList[T]): RList[T] = {
      remainingList match {
        case RNil => acc
        case ::(head, tail) if f(head) => filterTailRec(tail, head :: acc)
        case ::(head, tail) if !f(head) => filterTailRec(tail, acc)
      }
    }
    filterTailRec(this, RNil).reverse
  }

  /**
   *
   *  Medium problems
   */
  override def rle: RList[(T, Int)] = {
    @tailrec
    def rleTailRec(remaining: RList[T], acc: RList[(T, Int)], counter: Int): RList[(T, Int)] = {
      remaining match {
        case RNil => acc
        case ::(head, tail) if tail.isEmpty || head != tail.head  => rleTailRec(tail, (head, counter + 1) :: acc, 0)
        case ::(head, tail) if head == tail.head => rleTailRec(tail, acc, counter + 1)
      }
    }
    rleTailRec(this, RNil, 0).reverse
  }

  override def duplicateEach(k: Int): RList[T] = {
    @tailrec
    def duplicateEachTailRec(remaining: RList[T], acc: RList[T], occurrence: Int): RList[T] = {
      remaining match {
        case RNil => acc
        case ::(head, tail) if occurrence == k => duplicateEachTailRec(tail, head :: acc, 1)
        case ::(head, _) if occurrence != k => duplicateEachTailRec(remaining, head :: acc, occurrence + 1)
      }
    }
    duplicateEachTailRec(this, RNil, 1).reverse
  }

  override def rotate(k: Int): RList[T] = {
    /** 1 2 3.rotate(4) = rotateTailRec([1, 2, 3], 0, RNil)
        rotateTailRec([2, 3], 1, 1:: RNil)
        rotateTailRec([3], 2, 2 :: 1:: RNil)
        rotateTailRec([],  2, 2 :: 1:: RNil)
       Complexity: O(Max(N, K))
     **/
    @tailrec
    def rotateTailRec(remaining: RList[T], howManyTimesRotated: Int, acc: RList[T]): RList[T] = {
      remaining match {
        case RNil if howManyTimesRotated != k  => rotateTailRec(this, howManyTimesRotated, RNil)
        case RNil if howManyTimesRotated == k  => acc.reverse
        case ::(head, tail) if howManyTimesRotated == k => head :: tail ++ acc.reverse
        case ::(head, tail) if howManyTimesRotated < k => rotateTailRec(tail, howManyTimesRotated + 1, head :: acc) // 2, 1
      }
    }
    rotateTailRec(this, 0, RNil)
  }

  /*
      Complexity - O(N * K)
   */
  override def sample(k: Int): RList[T] = {
    @tailrec
    def sampleTailRec(acc: RList[T], count: Int): RList[T] = {
      if(count == k) {
        acc
      } else {
        val random = Random.nextInt(this.length)
        sampleTailRec(this.apply(random) :: acc, count + 1)
      }
    }

    if(k < 0) RNil
    else sampleTailRec(RNil, 0)
  }

  /*
      Complexity - O(N * K)
   */
  def sampleElegant(k: Int): RList[T] =
    RList.from((1 to k).map(_ => Random.nextInt(this.length)).map(index => this.apply(index)))

  override def sorted[S >: T](ordering: Ordering[S]): RList[S] = {
    /*
       insertSorted(4, [], [1, 2, 3, 5])
       insertSorted(4, [1], [2, 3, 5])
       insertSorted(4, [2, 1], [3, 5])
       insertSorted(4, [3, 2, 1], [5])
       [3, 2, 1].reverse + 4 :: [%]
     */
    @tailrec
    def insertSorted(element: T, before: RList[T], after: RList[T]): RList[T] = {
      if(after.isEmpty || ordering.lteq(element, after.head)) {
           before.reverse ++ (element :: after)
         } else {
           insertSorted(element, after.head :: before, after.tail)
         }
    }

     @tailrec
     def insertSortTailrec(remaining: RList[T], acc: RList[T]): RList[T] = {
       if(remaining.isEmpty) acc
       else insertSortTailrec(remaining.tail, insertSorted(remaining.head, RNil, acc))
     }
    insertSortTailrec(this, RNil)
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
  val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  val anotherSmallList = 4 :: 5 :: 6 :: RNil
  val mediumList: RList[Int] = aSmallList ++ anotherSmallList

  val aLargeList = RList.from(1 to 10000)
  val oneToTen = RList.from(1 to 10)
  def testEasyProblem(): Unit = {
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

    val mediumList: RList[Int] = aSmallList ++ anotherSmallList
    println(mediumList)

    println(aLargeList.removeAt(13))

    //map
    println(aSmallList.map(_ * 5))

    //flatmap
    println(aLargeList.flatMap(x => x :: (2 * x) :: RNil))
    //filter
    println(aLargeList.filter(_ % 2 != 0))
  }
  def testMediumDifficultyProblem(): Unit = {
    val duplicateList = 1 :: 1 :: 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 4 :: RNil

    println(duplicateList.rle)

    println(aSmallList.duplicateEach(2))
    for {
      i <- 1 to 10
    } println(oneToTen.rotate(i))

    println(aLargeList.sample(15))

  }

  //testMediumDifficultyProblem()

  def testHardDifficultyProblem() = {
    val rList = 5 :: 4 :: 3 :: 2 :: 1 :: RNil
    implicit val ordering = Ordering.fromLessThan[Int](_ < _)

    println(rList.sorted(ordering))
    println(aLargeList.sample(10).sorted(ordering))
  }

  testHardDifficultyProblem()
}