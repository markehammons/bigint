package org.math

/**
 * Created with IntelliJ IDEA.
 * User: luft
 * Date: 5/5/13
 * Time: 5:49 PM
 */
object BigIntBenches {

  def time(f: => Unit) = {
    val start = System.currentTimeMillis()
    f
    System.currentTimeMillis() - start
  }



  def llmulbench(times: Int) = time(for(i <- 0 until times) BigInt.valueOf(i) * BigInt.valueOf(i)) -> time(for(i <- 0 until times) scala.math.BigInt(i) * i)

  def lladdbench(times: Int) = {
    val x = MyInt(Long.MaxValue)
    val y = scala.math.BigInt(Long.MaxValue)

    "MyInt took " +
      (time(((0 to times) foldLeft (x)) {case (a,b) => a+a}) - time(((0 to times) foldLeft (y)) {case (a,b) => a+a})) +
      " milliseconds more"
  }
}
