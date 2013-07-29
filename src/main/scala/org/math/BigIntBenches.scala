package org.math

/**
 * Created with IntelliJ IDEA.
 * User: luft
 * Date: 5/5/13
 * Time: 5:49 PM
 */
object BigIntBenches {

  def time(f: => Unit) = {
    val start = System.nanoTime()
    f
    System.nanoTime() - start
  }



  def llmulbench(times: Int) = {
    val x = MyInt(Long.MaxValue) ** 13
    val x_y = MyInt(Long.MaxValue)
    val y = scala.math.BigInt(Long.MaxValue).pow(13)
    val y_y = scala.math.BigInt(Long.MaxValue)
    time{for(i <- 0 to times) x * x_y} - time{for(i <- 0 to times) y*y_y}
  }

  def lladdbench(times: Int) = {
    val x = MyInt(Long.MaxValue)
    val y = scala.math.BigInt(Long.MaxValue)

    time(((0 to times) foldLeft (x)) {case (a,b) => a+a}) - time(((0 to times) foldLeft (y)) {case (a,b) => a+a})
  }

  def smallBench(times: Int) = {
    val x = MyInt(Long.MaxValue)
    val y = scala.math.BigInt(Long.MaxValue)

    val res = time{for(i <- 0 to times) x+x} - time{for(i <- 0 to times) y+y}
    res
  }

  def smallMulBench(times: Int) = {
    val x = MyInt(Long.MaxValue)
    val y = scala.math.BigInt(Long.MaxValue)
    val res = time{for(i <- 0 to times) x*x} - time{for(i <- 0 to times) y*y}

    res
  }

  def subKaraBench(times: Int) = {
    val x = MyInt(Long.MaxValue)
    val y = scala.math.BigInt(Long.MaxValue)
    val pt1 = time{ (0 to times).foreach(_ =>
      (0 until 25).foldLeft(x){case (a,b) => a * x}
    )}

    val pt2 = time{ (0 to times).foreach(_ =>
      (0 until 25).foldLeft(y){case (a,b) => a * y}
    )}

    pt1 - pt2
  }

  def bigMulBench(times: Int) = {
    val x = MyInt(Long.MinValue) ** 2
    val y = scala.math.BigInt(Long.MinValue).pow(2)
    val old = time{(0 to times).foldLeft(y){case(a,b) => a*y}}
    val res = time{(0 to times).foldLeft(x){case(a,b) => a*x}} - old
    res
  }



  def main(args: Array[String]) {

    case class Stats(times: Int, timesForFn: Int, fn: Int => Long) {
      val benchResults = (0 until times) map (_ => fn(timesForFn))
      lazy val avg = (benchResults reduce (_ + _)) / (times.toDouble * timesForFn)
      lazy val min = (benchResults sortWith (_ < _)).head / timesForFn.toDouble
      lazy val max = (benchResults sortWith (_ > _)).head / timesForFn.toDouble

      override def toString = s"\tavg delta: $avg/ns\n\t\tmax delta: $max/ns\n\t\tmin delta: $min/ns\n"
    }

    def avgOver(times: Int, fn: => Long) = ((0 until times) map (_ => fn) reduce (_+_)) / times.toDouble
    println(s"lladdBench: ${Stats(50,20000,lladdbench)}")
    println(s"Small bench: ${Stats(100,80000,smallBench)}")
    println(s"llmulbench: ${Stats(50,10000,llmulbench)}")
    println(s"smallMulBench: ${Stats(50,10000,smallMulBench)}")
    println(s"subKaraBench: ${Stats(50,10000,subKaraBench)}")
    //println(s"bigMulBench: ${(for(i <- 0 until 100) yield bigMulBench(10000)).reduce{_+_} / 100.0}")
  }
}
