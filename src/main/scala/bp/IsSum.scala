package bp

import scala.annotation.tailrec
import scala.io.StdIn
import bp.utility._
/**
 * Question 2:
 *
 * Given an infinite stream of positive integers and a target positive integer, implement a function which returns when and only when a combination of the elements of the stream seen so far sums to the target
 *
 * Example
 *
 * stream = [1,7,3,1,15,2,...]
 * target = 5
 *
 * returns at the 4th element because 1 + 3 + 1 = 5 (edited)
 */
object IsSum extends App {
  
  
  val mockStream = new InfiniteListIntStream(List[Int](1, 7, 3, 1, 15, 2))
  val consoleStream = new StdInIntStream  

  /**
   * returns the iterator count of the stream when the sum is reached, 
   * throws IllegalStateException should internal cache fill
   */
  def sumIndex(target: Int, stream: MockStream[Int]): Int = {

    @tailrec
    def setOfSums(previousIndex: Int = -1, sums: Set[Int] = Set.empty[Int]): (Int, Set[Int]) =
      if (sums(target)) (previousIndex, sums)
      else {
        val next = stream.next()
        setOfSums(previousIndex + 1,
          sums + next ++ sums.map((value) => value + next).filter((value) => value <= target))
      }
    
    try {
      val (indexWhenSumReached, _) = setOfSums()
      indexWhenSumReached
    } catch { case e: IllegalStateException => e.printStackTrace(); throw e }
  }
  
  // shows that target 5 is reached upon value index = 3
  assert (3 == (sumIndex(5, mockStream)))
  
  // try out the console stream by typing in consecutive numbers
  println(sumIndex(5, consoleStream))

}


