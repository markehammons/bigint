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

    val res = time(((0 to times) foldLeft (x)) {case (a,b) => a+a}) - time(((0 to times) foldLeft (y)) {case (a,b) => a+a})
    s"MyInt took ${res} milliseconds more"
  }

  def smallBench(times: Int) = {
    val x = MyInt(Long.MaxValue)
    val y = scala.math.BigInt(Long.MaxValue)

    val res = time{for(i <- 0 to times) x+x} - time{for(i <- 0 to times) y+y}
    s"My Int took ${res} milliseconds more"
  }

  def main(args: Array[String]) {
    println(s"lladdBench: ${lladdbench(1000000)}")
    println(s"Small bench: ${smallBench(1000000)}")
  }
}
