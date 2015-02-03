package at.ac.tuwien.utility

/**
 * Created by aldo on 02/11/14.
 */
object Profile {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000000d + " s")
    result
  }

}
