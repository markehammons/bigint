package org.math

import scala.util.Random

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


  def depthBench(digits: Int)(times: Int) = {
    val x = MyInt(10) ** (digits - 1)

    val factorLim = if(digits < 8) math.pow(10,digits-1).toInt else Int.MaxValue.toInt
    val factor = Random.nextInt(factorLim)
    val x_factor = x + MyInt(factor)
    val y = scala.math.BigInt(10).pow(digits - 1)
    val y_factor = y + factor

    time{
      var i = 0
      while(i < times) {
        x * x_factor; i += 1
      }
    } -> time{
      var i = 0
      while(i < times) {
        y * y_factor; i += 1
      }
    }
  }


  def llmulbench(times: Int) = {
    val x = MyInt(Long.MaxValue) ** 13
    val x_y = MyInt(Long.MaxValue)
    val y = scala.math.BigInt(Long.MaxValue).pow(13)
    val y_y = scala.math.BigInt(Long.MaxValue)
    time{for(i <- 0 to times) x * x_y} - time{for(i <- 0 to times) y*y_y}
  }

  def bigaddbench(times: Int) = {
    val x = MyInt(Long.MaxValue) ** 50
    val y = scala.math.BigInt(Long.MaxValue).pow(50)

    time{
      var y = x
      var i = 0
      while(i < times) {
        y + x; i += 1
      }
    } -> time{
      var z = y
      var i = 0
      while(i < times) {
        z + y; i += 1
      }
    }
  }

  def smallBench(times: Int) = {
    val x = MyInt(Long.MaxValue)
    val y = scala.math.BigInt(Long.MaxValue)

    val res = time{for(i <- 0 to times) x+x} -> time{for(i <- 0 to times) y+y}
    res
  }

  def smallSubBench(times: Int) = {
    val r = Random.nextLong()
    val oR = Random.nextLong()
    val x = MyInt(r)
    val y = scala.math.BigInt(r)

    time{
      var i = 0
      while(i < times) {
        MyInt(r) - MyInt(oR); i += 1
      }
    } -> time {
      var i = 0
      while(i < times) {
        BigInt(r) - BigInt(oR); i += 1
      }
    }

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

    case class Stats(times: Int, timesForFn: Int, fn: Int => (Long,Long)) {
      /*warmup*/
      val warmup = (0 until times) map (_ => fn(timesForFn))
      val benchResults = (0 until times) map (_ => fn(timesForFn))
      lazy val avgTime = {
        val x = benchResults reduce ( (a: (Long,Long),b:(Long,Long)) => (a._1+b._1) -> (a._2 + b._2))
        (x._1 / benchResults.length) -> (x._2 /benchResults.length)
      }
      lazy val deltas = benchResults map {case (a,b) => a - b}
      lazy val ratios = benchResults map {case (a,b) => b.toDouble/a.toDouble}
      lazy val avgRatio = (ratios reduce(_ + _)) / ratios.length.toDouble
      lazy val avg = (deltas reduce (_ + _)) / (times.toDouble * timesForFn)
      lazy val min = (deltas sortWith (_ < _)).head / timesForFn.toDouble
      lazy val max = (deltas sortWith (_ > _)).head / timesForFn.toDouble

      override def toString = s"\tavg delta: $avg\n\t\tratio: $avgRatio\n\t\tmax delta: $max/ns\n\t\tmin delta: $min/ns\n\t\tnew - old: $avgTime\n"
    }

    def avgOver(times: Int, fn: => Long) = ((0 until times) map (_ => fn) reduce (_+_)) / times.toDouble

    //List(10,25,50,75,100,250,500) foreach {i => Stats(250,20000,depthBench(i))}
    List(10,15,20,25,50,75,100,150,200,250,500) foreach (i => println(s"${i}d: ${Stats(100,10000,depthBench(i))}"))

    println(s"bigaddBench: ${Stats(100,18000,bigaddbench)}")
    println(s"Small bench: ${Stats(100,80000,smallBench)}")
    println(s"Small Subtraction Bench: ${Stats(1000,80000,smallSubBench)}")
    /*println(s"llmulbench: ${Stats(50,10000,llmulbench)}")
    println(s"smallMulBench: ${Stats(50,10000,smallMulBench)}")
    println(s"subKaraBench: ${Stats(50,10000,subKaraBench)}")*/
    //println(s"bigMulBench: ${(for(i <- 0 until 100) yield bigMulBench(10000)).reduce{_+_} / 100.0}")
  }
}
