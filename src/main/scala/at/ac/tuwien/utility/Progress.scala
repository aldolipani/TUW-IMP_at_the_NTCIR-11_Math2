package at.ac.tuwien.utility

/**
 * Created by aldo on 02/11/14.
 */
object Progress {
  var cnt: Long = 0
  var cntTime: Long = 0

  def log[A](a: => A): A = {
    cnt = cnt + 1
    cntTime = cntTime - System.nanoTime
    val result = a
    cntTime = cntTime + System.nanoTime

    if (cnt % 10000 == 0) {
      val secs = cntTime.toFloat / 1000000000
      println(cnt / 10000 + "0k performed in %f s".format(secs))
      cntTime = 0
    }
    result
  }
}