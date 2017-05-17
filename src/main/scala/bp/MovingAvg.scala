package bp

/**
 *
 * app for finding the moving avg across an array 
 */
object MovingAvg extends App {

  /**
   * returns
   * 1.) array of moving averages across the input value windows
   * 2.) or an empty array if we don't have enough input values to satisfy window size
   *
   */
  def movingAvg(values: Array[Int], window: Int): Array[Double] =
    doMovingAvg(values, window) match {
      case None    => Array[Double]()
      case Some(a) => a
    }

  private def doMovingAvg(values: Array[Int], window: Int): Option[Array[Double]] =
    if (window > values.length) None
    else {

      def avgWithOffset(offset: Int): Double = {
        val slice = values.slice(offset, (offset + window))
        val avg = slice.sum / window

        // for inspection of window averages...
        //println (s"${slice.mkString(",")} => $avg")

        avg
      }

      // perform the avgWithOffset across the window for valid offsets
      Some((for (i <- (0 to values.length - window)) yield avgWithOffset(i)).toArray)
    }

  println(s"${movingAvg(Array[Int](1, 2, 3, 4, 5, 6, 7), 3).mkString(",")}")

}
