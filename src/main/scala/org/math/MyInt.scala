package org.math

import scala.annotation.tailrec


/**
 * Created with IntelliJ IDEA.
 * User: luft
 * Date: 5/5/13
 * Time: 7:58 PM
 */

object MyInt {

  private val lowerHalf = 0x0000FFFF

  private val higherHalf = 0xFFFF0000

  private final val karatsubaThreshold = 25

  private final val toomCookThreshold = 38

  private final val karatsubaSquareThreshold = 45

  private final val toomCookSquareThreshold = 70

  protected def apply(x: Array[Long]): MyInt = new MyInt(x)

  @inline
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

  private val lower = 0x00000000FFFFFFFFL

  @inline private def ult(a: Long, b: Long) = if((a ^ b) < 0) (a >>> 1) < (b >>> 1) else a < b

  @inline
  private def overflowDetect(r: Long, a: Long, b: Long) = ult(r, a)

  /*@inline
  private def overcheck_old(a: Long, b: Long) = ((((a & mask_32) + (b & mask_32)) >>> 32) + (a >>> 32) + (b >>> 32)) >>> 32*/

  @inline final case class DoubleLong(l: (Long,Long)) extends AnyVal {
    @inline final def +(y: Long) = {
      val r = l._2 + y
      DoubleLong(l._1 + (if(ult(r,y)) 1 else 0), r)
    }

    @inline final def *(b: Long) = {
      val a = l._2
      val (aUpper,aLower) = (a >>> 32, a & lower)
      val (bUpper, bLower) = (b >>> 32, b & lower)
      val (t) = aUpper * bUpper
      val u = aUpper * bLower
      val v = bUpper * aLower
      val w = aLower * bLower
      val (uLower, vLower) = ((u & lower) << 32, (v & lower) << 32)
      val d1 = DoubleLong.add(w,uLower)
      val d2 = d1 + vLower
      val down = d2 + l._1
      val up = t + (u >>> 32) + (v >>> 32) + down.l._1
      DoubleLong(up,down.l._2)
    }

    @inline final def res = l._2
    @inline final def carry = l._1
    @inline final override def toString = l.toString()
  }

  object DoubleLong {
    @inline final def apply(c: Long, r: Long): DoubleLong = DoubleLong((c,r))
    @inline final def apply(r: Long): DoubleLong = DoubleLong((0l,r))
    @inline final def add(x: Long, y: Long) = {
      val r = x + y
      DoubleLong(if(ult(r,x)) 1 else 0, r)
    }
    @inline final def mul(a: Long, b: Long) = {
      val (aUpper,aLower) = (a >>> 32, a & lower)
      val (bUpper, bLower) = (b >>> 32, b & lower)
      val (t) = aUpper * bUpper
      val u = aUpper * bLower
      val v = bUpper * aLower
      val w = aLower * bLower
      val (uLower, vLower) = ((u & lower) << 32, (v & lower) << 32)
      val d1 = DoubleLong.add(w,uLower)
      val down = d1 + vLower
      val up = t + (u >>> 32) + (v >>> 32) + down.carry
      DoubleLong(up,down.res)
    }
  }

  @inline// a seemingly perfect long*long multiplier
  private def longMul(a: Long, b: Long, c: Long, arr: Array[Long], pos: Int) = {
    val (aUpper,aLower) = (a >>> 32, a & lower)
    val (bUpper, bLower) = (b >>> 32, b & lower)
    val (t) = aUpper * bUpper
    val u = aUpper * bLower
    val v = bUpper * aLower
    val w = aLower * bLower
    val (uLower, vLower) = ((u & lower) << 32, (v & lower) << 32)
    val d1 = w + uLower
    val d2 = d1 + vLower
    val down = d2 + c
    arr(pos) = down
    var carry = if(ult(d1, w)) 1 else 0
    carry += (if(ult(d2, d1)) 1 else 0)
    carry += (if(ult(down, d2)) 1 else 0)
    t + (u >>> 32) + (v >>> 32) + carry
  }

  @inline
  private def longMul(a: Long, b: Long, c: Long, d: Long, arr: Array[Long], pos: Int) = {
    val (aUpper, aLower) = (a >>> 32, a & lower)
    val (bUpper, bLower) = (b >>> 32, b & lower)
    val t = aUpper * bUpper
    val u = aUpper * bLower
    val v = bUpper * aLower
    val w = aLower * bLower
    val (uLower, vLower) = ((u & lower) << 32, (v & lower) << 32)
    val d1 = w + uLower
    val d2 = d1 + vLower
    val d3 = d2 + c
    val down = d3 + d
    arr(pos) = down
    var carry = if(ult(d1, w)) 1 else 0
    carry += (if(ult(d2, d1)) 1 else 0)
    carry += (if(ult(d3, d2)) 1 else 0)
    carry += (if(ult(down, d3)) 1 else 0)
    t + (u >>> 32) + (v >>> 32) + carry
  }


  def multiplyByLong(x: Array[Long], y: Long, sign: Long): MyInt = {
    val z = multiplyByLong(x,y)
    z(0) = sign
    MyInt(z)
  }

  final private def multiplyByLong(x: Array[Long], y: Long): Array[Long] = {
    if(x(1) == 1)
      Array(0,y)
    else if(y == 1) {
      val a = java.util.Arrays.copyOf(x, x.length)
      a(0) = 0
      a
    } else {
      val xlen = x.length

      var rmag = new Array[Long](xlen + 1)

      var carry = 0l
      var rStart = xlen - 1

      while(rStart > 0) {
        carry = longMul(x(rStart), y, carry, rmag, rStart+1)
        rStart -= 1
      }

      if(carry == 0l)
        rmag = java.util.Arrays.copyOfRange(rmag, 1, rmag.length)
      else {
        rmag(0) = 0
        rmag(1) = carry
      }

      rmag
    }

  }

  private def shiftLeft64(x: Array[Long], times: Int) = {
    val arr = new Array[Long](x.length + times)
    Array.copy(x,0,arr,0,x.length)
    arr
  }


  final private def multiplyToLen(x: Array[Long], xlen: Int, y: Array[Long], ylen: Int, res: Array[Long]): Array[Long] = {
    val xstart = xlen - 1
    val ystart = ylen - 1

    val z = new Array[Long](xlen-1+ylen)

    var carry = 0l
    var j = ystart
    var k = ystart+xstart
    while(j > 0) {
      carry = longMul(y(j), x(xstart),carry,z,k)
      j -= 1; k -= 1
    }

    z(xstart) = carry

    var i = xstart-1
    while(i > 0) {
      carry = 0
      j = ystart
      k = ystart+i
      while(j > 0) {
        carry = longMul(y(j), x(i), z(k), carry, z, k)
        j-=1;k-=1
      }
      z(i) = carry
      i -= 1
    }

    z
  }

  /*def multiplyToLen(x: Array[Long], xlen: Int, y: Array[Long], ylen: Int, res: Array[Long]): Array[Long] = {
    var res = ZERO.x

    var i = ylen - 1
    while(i > 0) {
      res = add(res,shiftLeft64(multiplyByLong(x,y(i)), ylen - i - 1))
      i -= 1
    }
    res
  }*/

  def multiplyKaratsuba(x: MyInt, y: MyInt) =  ???

  def multiplyToomCook3(x: MyInt, y: MyInt) = ???

  def multiplySchoenhageStrassen(x: MyInt, y: MyInt) = ???

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

  private val ZERO = new MyInt(Array(0l,0l))

  private val ONE = MyInt(1)

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

      difference = if(ult(res,min)) 1 else 0
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

  private final def add(a: Array[Long], b: Array[Long]): Array[Long] = {
    val (x,y) = if(a.length < b.length) (b,a) else (a,b)

    var xIndex = x.length
    var yIndex = y.length
    val result = new Array[Long](xIndex)
    result(0) = x(0)
    var sum = 0l

    while(yIndex > 1) {
      xIndex -= 1; yIndex -= 1
      val (a,b) = (x(xIndex), y(yIndex))
      val res = b + a + sum
      result(xIndex) = res
      sum = if(ult(res, a)) 1 else 0
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
      bigger
    } else result
  }



}

class MyInt(val x: Array[Long]) extends AnyVal {
  import MyInt._

  override def toString = {
  (if(signum == -1) "-" else "") + x(1).toBinaryString +
    (if(x.length > 2) (2 until x.length).map({i =>
      val y = x(i).toBinaryString
      (("0" * (64 - y.length)) + y)
    }).foldLeft ("") (_+_) else "")
  }

  def signum = x(0)

  final def +(value: MyInt): MyInt = {
    val (sig,oSig) = (signum, value.signum)
    if(oSig == 0)
      return this
    else if(sig == 0)
      return value
    else if(oSig == sig)
      return MyInt(add(x, value.x))

    val cmp = compareMagnitude(value)
    if(cmp == 0)
      return ZERO

    val resultMag = trustedStripLeadingZeroInts(if(cmp > 0) subtract(x, value.x) else subtract(value.x, x))

    resultMag(0) = if(cmp == sig) 1 else -1
    MyInt(resultMag)
  }

  def size = x.length

  private final def compareMagnitude(value: MyInt): Int = {
    val m1 = x
    val len1 = m1.length
    val m2 = value.x
    val len2 = m2.length
    if(len1 < len2)
      return -1
    else if(len1 > len2)
      return 1
    var i = 1
    while(i < len1) {
      val a = m1(i)
      val b = m2(i)
      if(a != b) return if(ult(a,b)) -1 else 1
      i += 1
    }
    return 0
  }

  def *(v: MyInt): MyInt = {
    val (sig,oSig) = (signum, v.signum)

    if(sig == 0 || oSig == 0)
      return ZERO

    val xlen = x.length
    val ylen = v.x.length

    if((xlen < karatsubaThreshold) || (ylen < karatsubaThreshold)) { //TODO: THIS CODE IS FATALLY FLAWED, toomCook
    // will not be active unless both xlen and ylen are larger than karatsubaThreshold
      val resultSign = if(sig == oSig) 1 else -1
      if(ylen == 2) {
        return multiplyByLong(x, v.x(1), resultSign)
      }
      if(x.length == 2) {
        return multiplyByLong(v.x, x(1), resultSign)
      }

      var result = multiplyToLen(x, xlen, v.x, ylen, null)

      result = trustedStripLeadingZeroInts(result)
      result(0) = resultSign
      return MyInt(result)
    }
    else
      if((xlen < toomCookThreshold) && (ylen < toomCookThreshold))
        return multiplyKaratsuba(this, v)
      else
        ??? //TODO shouldMultiplySchoenhageStrassen
  }

  private def shiftLeft64(times: Int) = {
    val arr = new Array[Long](x.length + times)
    Array.copy(x,0,arr,0,x.length)
    MyInt(arr)
  }

  def *(v: Long): MyInt = this * MyInt(v)

  def **(v: Int): MyInt = if(v > 1) this * (this ** (v-1)) else this
}
