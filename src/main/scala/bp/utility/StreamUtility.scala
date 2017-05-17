package bp.utility

import java.io.EOFException
import scala.annotation.tailrec

trait MockStream[I] {
  def next(): I
}

/**
 * trait for reading from a MockStream
 */
trait MockStreamReader[I] {
  
  /**
   * read from a stream until success
   * return value associated with success 
   */
  def read(stream: MockStream[I], success: (I) => (Boolean, I)): Option[I] = {
    while (!exit._1) {
      val i = stream.next()
      success(i) match {
        case (true, a)      => return Some(a)
        case (false, _)     => { /* read next input */ }
      }
    }
    None
  }
  
  /**
   * return (true, "$reason") iff exit desired (false, _) if not desired
   */
  def exit(): (Boolean, String)
}

/**
 * MockStreamReader implementation for console Integers,
 * exits with non numeric character input
 */
class StdInIntStream extends MockStream[Int] with MockStreamReader[Int] {
  var eflag = false
  
  @tailrec
  final override def next(): Int = {
    if (exit._1) throw new EOFException(s"exit invoked: ${exit._2}")
    try {
      return scala.io.StdIn.readInt()
    } catch {
      /* any key besides a number causes us to exit */
      case nfe: NumberFormatException => eflag = true 
      case e: EOFException            => Thread.sleep(500)
    }
    next() 
  }
  
  def exit(): (Boolean, String) = 
    if (eflag) (eflag, "non numeric key pressed") 
    else (eflag, "")
  
}

/**
 * MockStreamReader implementation for an List[Int] 
 * repeats forever up until allowedIterations 
 */
class InfiniteListIntStream(val delegate: List[Int], allowedIterations: Int = 1000) extends MockStream[Int] 
  with MockStreamReader[Int] {
    var i = -1
    var waterMark = 0
    val apply = (index: Int) => { 
      waterMark += 1
      delegate(index % delegate.length) 
    }
    
    def next():Int = 
      if (exit._1) throw new IllegalStateException(s"exit invoked: ${exit._2}")
      else { i += 1; apply(i) }
    
    def exit(): (Boolean, String) = 
      if (waterMark > allowedIterations) 
        (true, "upper bound reached on number of iterations") 
      else (false, "")
      
}
