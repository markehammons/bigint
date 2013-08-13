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

  private final def trustedStripLeadingZeroInts(a: Array[Long]) = {
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

  private val stripSign = 0x7FFFFFFFFFFFFFFFL


  final def ult(a: Long, b: Long) = a + Long.MinValue < b + Long.MinValue

  final def ult2(a: Long, b: Long) = if(a + Long.MinValue < b + Long.MinValue) 1l else 0l

  final def ult4(a: Long, b: Long) = (((a >>> 63) - (b >>> 63)) | (((a& stripSign) - (b & stripSign)) & ((b ^ a) ^ -1))) >>> 63



  // a seemingly perfect long*long multiplier
  /*final def longMul(a: Long, b: Long, c: Long, arr: Array[Long], pos: Int) = {
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
    var carry = ult2(d1, w)
    carry += ult2(d2, d1)
    carry += ult2(down, d2)
    t + (u >>> 32) + (v >>> 32) + carry
  }*/

  /*final def longMul(a: Long, b: Long, c: Long, d: Long, arr: Array[Long], pos: Int) = {
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
    var carry = ult2(d1,w)
    carry += ult2(d2,d1)
    carry += ult2(d3,d2)
    carry += ult2(down,d3)
    t + (u >>> 32) + (v >>> 32) + carry
  }*/

  def shiftLeft(mag: Array[Long], n: Int): Array[Long] = {
    val nLongs = n >>> 6
    val nBits = n & 0x3f
    val magLen = mag.length

    if(nBits == 0) {
      val newMag = new Array[Long](magLen + nLongs)
      System.arraycopy(mag,1,newMag,1,magLen - 1)
      newMag
    } else {
      var i = 1
      val nBits2 = 64 - nBits
      val highBits = mag(1) >>> nBits2
      val newMag = if(highBits != 0) {
        val t = new Array[Long](magLen + nLongs + 1)
        t(i) = highBits; i += 1
        t
      } else {
        new Array[Long](magLen + nLongs)
      }
      var j = 1
      while(j < magLen-1) {
        newMag(i) = mag(j) << nBits | mag(j) >>> nBits2; i += 1; j += 1
      }
      newMag(i) = mag(j) << nBits
      newMag
    }
  }

  def shiftRight(mag: Array[Long], n: Int): Array[Long] = ???

  def oldLongMul(a:Long, b: Long, c: Long): (Long,Long) = {
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
    var carry = ult2(d1, w)
    carry += ult2(d2, d1)
    carry += ult2(down, d2)
    (t + (u >>> 32) + (v >>> 32) + carry, down)
  }

  final def properLongMul(a: Long, b: Long, carry: Long): (Long,Long) = {
    val (aUpper, aLower) = (a >>> 32, a & lower)
    val (bUpper, bLower) = (b >>> 32, b & lower)

    val w = aLower * bLower + (carry & lower)
    val v = bUpper * aLower + (w >>> 32) + (carry >>> 32)
    val u = aUpper * bLower + (v & lower)
    val t = aUpper * bUpper + (v >>> 32) + (u >>> 32)
    val down = (u << 32) + (w & lower)
    (t,down)
  }

  final def longMul(a: Long, b: Long, carry: Long, arr: Array[Long], p: Int) = {
    val aUpper = a >>> 32
    val aLower = a & lower
    val bUpper = b >>> 32
    val bLower = b & lower

    val w = aLower * bLower + (carry & lower)
    val v = bUpper * aLower + (w >>> 32) + (carry >>> 32)
    val u = aUpper * bLower + (v & lower)
    val t = aUpper * bUpper + (v >>> 32) + (u >>> 32)
    arr(p) = (u << 32) + (w & lower)
    t
  }

  final def longMul(a: Long, b: Long, carry: Long, carry2: Long, arr: Array[Long], p: Int) = {
    val aUpper = a >>> 32
    val aLower = a & lower
    val bUpper = b >>> 32
    val bLower = b & lower

    val w = aLower * bLower + (carry & lower) + (carry2 & lower)
    val v = bUpper * aLower + (w >>> 32) + (carry >>> 32)
    val u = aUpper * bLower + (v & lower) + (carry2 >>> 32)
    val t = aUpper * bUpper + (v >>> 32) + (u >>> 32)
    arr(p) = (u << 32) + (w & lower)
    t
  }

  final private def multiplyByLong(x: Array[Long], xlen: Int, y: Long, sign: Long): MyInt = {
    val z = multiplyByLong(x,xlen,y)
    z(0) = sign
    MyInt(z)
  }

  final private def multiplyByLong(x: Array[Long], xlen: Int, y: Long): Array[Long] = {
    if(x == ONE.x)
      Array(0,y)
    else if(y == 1l) {
      val a = java.util.Arrays.copyOf(x, x.length)
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

      if(carry == 0l) {
        val n = new Array[Long](rmag.length - 1)
        System.arraycopy(rmag,2, n,1,n.length-1)
        rmag = n
      } else {
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

    val z = new Array[Long](xstart+ylen)

    var carry = 0l
    var j = ystart
    val x_xstart = x(xstart)

    while(j > 0) {
      carry = longMul(y(j), x_xstart, carry, z, j+xstart)
      j -= 1
    }

    z(xstart) = carry

    /*def firstLoop(j: Int, carry: Long): Long = {
      if(j > 0) firstLoop(j-1, longMul(y(j), x_xstart, carry, z, j+xstart))
      else carry
    }

    z(xstart) = firstLoop(ystart,0)*/

    def outerLoop(i: Int) {
      val x_i = x(i)
      @inline def innerLoop(j: Int, carry: Long): Long = {
        if(j > 0) innerLoop(j-1, longMul(y(j), x_i, z(j+i), carry, z, j+i))
        else carry
      }
      if(i > 0) {
        z(i) = innerLoop(ystart, 0)
        outerLoop(i - 1)
      }
    }


    outerLoop(xstart - 1)

    /*while(i > 0) {
      implicit val x_i = x(i)
      z(i) = innerLoop(ystart, 0)
      i -= 1
    }*/

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



  def multiplyKaratsuba(x: MyInt, y: MyInt) =  {
    val xLen = x.x.length - 1
    val yLen = y.x.length - 1

    val half = (math.max(xLen, yLen) + 1) / 2

    val xl = x.getLower(half)
    val xh = x.getUpper(half)
    val yl = y.getLower(half)
    val yh = y.getUpper(half)

    val p1 = xh * yh
    val p2 = xl * yl

    val p3 = (xh + xl) * (yh + yl)

    val result = (p1 << (128 * half)) + ((p3 - p1 - p2) << (64 * half)) + p2

    if(x.signum != y.signum) -result
    else result
  }

  def multiplyToomCook3(x: MyInt, y: MyInt) = {???; ZERO}



  def multiplySchoenhageStrassen(x: MyInt, y: MyInt) = {???; ZERO}

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
      result(minIndex) = min - (sub + difference)

      difference = ult2(min,sub)
    }

    var borrow = difference != 0

    while(minIndex > 1) {
      minIndex -= 1
      result(minIndex) = minuend(minIndex) - (if(borrow) 1 else 0)
      if(borrow) borrow = result(minIndex) == -1
    }

    result
  }

  private final def add(a: Array[Long], b: Array[Long]): Array[Long] = {
    val x = if(a.length < b.length) b else a
    val y = if(a.length < b.length) a else b

    var xIndex = x.length
    var yIndex = y.length
    val result = new Array[Long](xIndex)
    result(0) = x(0)
    var sum = 0l

    while(yIndex > 1) {
      xIndex -= 1; yIndex -= 1
      val a = x(xIndex)
      val b = y(yIndex)
      val res = b + a + sum
      result(xIndex) = res
      sum = ult2(res,a)
    }

    var carry = sum != 0
    while(xIndex > 1) {
      xIndex -= 1
      result(xIndex) = x(xIndex) + (if (carry) 1 else 0)
      if(carry) carry = result(xIndex) == 0
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

final class MyInt(val x: Array[Long]) extends AnyVal {
  import MyInt._

  override def toString = {
  (if(signum == -1) "-" else "") + x(1).toBinaryString +
    (if(x.length > 2) (2 until x.length).map({i =>
      val y = x(i).toBinaryString
      (("0" * (64 - y.length)) + y)
    }).foldLeft ("") (_+_) else "")
  }

  def signum = x(0)

  def +(value: MyInt): MyInt = {
    val sig = x(0)
    val oSig = value.x(0)
    if(oSig == 0)
      this
    else if(sig == 0)
      value
    else if(oSig == sig)
      MyInt(add(x, value.x))
    else {
      val cmp = compareMagnitude(value)
      if(cmp == 0)
        ZERO
      else {
        val resultMag = trustedStripLeadingZeroInts(if(cmp > 0) subtract(x, value.x) else subtract(value.x, x))

        resultMag(0) = if(cmp == sig) 1 else -1
        MyInt(resultMag)
      }
    }
  }

  def -(value: MyInt) = {
    val sig = x(0)
    val oSig = value.x(0)
    if(oSig == 0)
      this
    else if(sig == 0)
      -value
    else if(sig != oSig) {
      val r = add(x, value.x)
      r(0) = sig
      MyInt(r)
    }
    else {
      val cmp = compareMagnitude(value)
      if(cmp == 0)
        ZERO
      else {
        val resultMag = if(cmp > 0) subtract(x, value.x) else subtract(value.x, x)
        resultMag(0) = if(cmp == sig) 1 else -1
        MyInt(trustedStripLeadingZeroInts(resultMag))
      }
    }
  }

  def unary_- = {
    if(this == ZERO) this
    else {
      val nMag = java.util.Arrays.copyOf(x, x.length)
      nMag(0) = if(x(0) == 1) -1 else 1
      new MyInt(nMag)
    }
  }

  def size = x.length

  private def compareMagnitude(value: MyInt): Int = {
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
    val sig = x(0)
    val oSig= v.x(0)

    val xlen = x.length
    val ylen = v.x.length

    if(sig == 0 || oSig == 0)
      ZERO
    else if((xlen < karatsubaThreshold) && (ylen < karatsubaThreshold) || (ylen < 4) || (xlen < 4)) { //TODO: THIS CODE IS FATALLY FLAWED, toomCook
    // will not be active unless both xlen and ylen are larger than karatsubaThreshold
      val resultSign = if(sig == oSig) 1 else -1
      if(xlen + ylen > 4) {
        val result = trustedStripLeadingZeroInts(multiplyToLen(x, xlen, v.x, ylen, null))

        result(0) = resultSign
        MyInt(result)
      } else if (ylen == 2) {
        multiplyByLong(x, xlen, v.x(1), resultSign)
      }
      else {
        multiplyByLong(v.x, ylen, x(1), resultSign)
      }
    }
    else
      if((xlen < toomCookThreshold) && (ylen < toomCookThreshold))
        multiplyKaratsuba(this, v)
      else
        multiplySchoenhageStrassen(this,v) //TODO shouldMultiplySchoenhageStrassen
  }

  private def getLower(n: Int) = {
    val len = x.length

    if(len - 1 <= n)
      this

    val lowerLongs = new Array[Long](n+1)
    System.arraycopy(x, len-n, lowerLongs, 1, n) //TODO: this can probably be optimized with a specialized stripleadingzeros
    lowerLongs(0) = 1

    new MyInt(trustedStripLeadingZeroInts(lowerLongs))
  }

  private def getUpper(n: Int) = {
    val len = x.length - 1

    if(len <= n)
      ZERO

    val upperLen = (len - n) + 1
    val upperLongs = new Array[Long](upperLen)
    System.arraycopy(x, 1, upperLongs, 1, upperLen-1)
    upperLongs(0) = 1

    new MyInt(trustedStripLeadingZeroInts(upperLongs))
  }

  def <<(n: Int) = {
    if(x(0) == 0)
      ZERO
    else if(n == 0)
      this
    else if(n < 0) {
      if (n == Int.MinValue) {
        throw new ArithmeticException("Shift distance of Int.MinValue not supported.")
      } else {
        this >> -n
      }
    } else {
      val newMag = shiftLeft(x, n)

      newMag(0) = x(0)

      MyInt(newMag)
    }
  }

  private def getToomSlice(lowerSize: Int, upperSize: Int, slice: Int, fullsize: Int) = ???

  def >>(n: Int): MyInt = ???

  def *(v: Long): MyInt = this * MyInt(v)

  def **(v: Int): MyInt = if(v > 0) this * (this ** (v-1)) else ONE
}
