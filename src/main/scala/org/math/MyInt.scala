package org.math


/**
 * Created with IntelliJ IDEA.
 * User: luft
 * Date: 5/5/13
 * Time: 7:58 PM
 */

object MyInt {

  //DON'T USE - SLOW
  /*implicit class RichULong(val l: Long) extends AnyVal {
    @inline
    def <|(oL: Long) = if(l < 0 || oL < 0) (l >>> 1) < (oL >>> 1) else l < oL

    @inline
    def xor(a: Boolean, b: Boolean) = {
      val x = nand(a,b)
      val y = nand(a,x)
      val z = nand(b,x)
      nand(y,z)
    }

    @inline
    def nand(a: Boolean, b: Boolean) = !(a && b)
  }*/

  private val cleanBit = Long.MaxValue

  private class MyIntImpl(val x: Array[Long]) extends AnyVal with MyInt {}

  protected def apply(x: Array[Long]): MyInt = new MyIntImpl(x)

  private def trustedStripLeadingZeroInts(a: Array[Long]) = {
    val vlen = a.length
    var keep = 1

    while(keep < vlen && a(keep) == 0) {
      keep += 1
    }

    val r = if(keep == 1) a else java.util.Arrays.copyOfRange(a, keep-1, vlen)
    r(0) = a(0)
    r
  }

  @inline
  private def overcheck(res: Long) = res >>> 63

  @inline
  private def ult(a: Long, b: Long) = if(a < 0 && b < 0) (a >>> 1) < (b >>> 1) else a < b

  @inline
  private def overflowDetect(r: Long, a: Long, b: Long) = (a != 1 && b != 1) && ult(r, a)


  /*@inline
  private def overcheck_old(a: Long, b: Long) = ((((a & mask_32) + (b & mask_32)) >>> 32) + (a >>> 32) + (b >>> 32)) >>> 32*/

  def multiplyByLong(x: Array[Long], y: Long, sign: Long) = ??? /*{
    val xlen = x.length

    val rmag = new Array[Long](xlen + 1)
    val carry = 0l
    val rstart = rmag.length - 1
    for(i <- xlen -1 to 1 by -1) {
      val product = x(i) *
    }

  } */

  def apply(v: Long): MyInt = {

    var value = v
    if(value == 0)
      return ZERO
    var signum = 0
    if (value < 0) {
      value = -value
      signum = -1
    } else {
      signum = 1
    }
    MyInt(Array(signum,value))
  }

  private val ZERO = new MyIntImpl(Array(0l,0l))

  private def subtract(minuend: Array[Long], subtrahend: Array[Long]): Array[Long] = {
    var minIndex = minuend.length
    val result = new Array[Long](minIndex)
    var subIndex = subtrahend.length
    var difference = 0l

    while(subIndex > 1) {
      minIndex -= 1
      subIndex -= 1
      val min = minuend(minIndex)
      val sub = subtrahend(subIndex)
      val res = min - sub + difference
      result(minIndex) = res

      difference = if(overflowDetect(res,min,sub)) 1 else 0
    }

    var borrow = difference != 0

    while(minIndex > 1 && borrow) {
      minIndex -= 1
      result(minIndex) = minuend(minIndex) - 1
      borrow = result(minIndex) == -1
    }

    while(minIndex > 1) {
      minIndex -= 1
      result(minIndex) = minuend(minIndex)
    }

    result
  }

  private def add(a: Array[Long], b: Array[Long]): Array[Long] = {
    val (x,y) = if(a.length < b.length) (b,a) else (a,b)

    var xIndex = x.length
    var yIndex = y.length
    val result = new Array[Long](xIndex)
    result(0) = x(0)
    var sum = 0l
    if(yIndex == 2) {
      xIndex -= 1
      val (a,b) = (x(xIndex), y(1))
      val res = x(xIndex) + y(1)
      sum = if(overflowDetect(res, a, b)) 1 else 0
      result(xIndex) = res
    } else {
      while(yIndex > 1) {
        xIndex -= 1; yIndex -= 1
        val (a,b) = (x(xIndex), y(yIndex))
        val res = a + b + sum
        result(xIndex) = res
        sum = if(overflowDetect(res, a,b)) 1 else 0
      }
    }

    var carry = sum != 0
    while(xIndex > 1 && carry) {
      xIndex -= 1
      result(xIndex) = x(xIndex) + 1
      carry = result(xIndex) == 0
    }

    while(xIndex > 1) {
      xIndex -= 1
      result(xIndex) = x(xIndex)
    }

    if(carry) {
      val bigger = new Array[Long](result.length + 1)
      System.arraycopy(result, 0, bigger, 1, result.length)
      bigger(0) = bigger(1)
      bigger(1) = 0x01
      return bigger
    }
    result
  }



}

trait MyInt extends Any {
  import MyInt._
  protected def x: Array[Long]

  override def toString = {
  (if(signum == -1) "-" else "") + x(1).toBinaryString +
    (if(x.length > 2) (2 until x.length).map({i =>
      val y = x(i).toBinaryString
      (("0" * (64 - y.length)) + y)
    }).foldLeft ("") ((a,b) => a + b) else "")
  }

  def signum = x.head

  def +(value: MyInt): MyInt = {
    if(value.signum == 0)
      return this
    else if(signum == 0)
      return value
    else if(value.signum == signum)
      return MyInt(add(x, value.x))

    val cmp = compareMagnitude(value)
    if(cmp == 0)
      return ZERO

    val resultMag = trustedStripLeadingZeroInts(if(cmp > 0) subtract(x, value.x) else subtract(value.x, x))

    resultMag(0) = if(cmp == signum) 1 else -1
    MyInt(resultMag)
  }

  def size = x.length


  final def compareMagnitude(value: MyInt): Int = {
    val m1 = x
    val len1 = m1.length
    val m2 = value.x
    val len2 = m2.length
    if(len1 < len2)
      return -1
    else if(len1 > len2)
      return 1
    for(i <- 1 until len1) {
      val a = m1(i)
      val b = m2(i)
      val x = -((a | b) >> 63)
      if(a != b) return if((a >>> x) < (b >>> x)) -1 else 1
    }
    return 0
  }


}
