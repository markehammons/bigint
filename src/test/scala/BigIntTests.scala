/**
 * Created with IntelliJ IDEA.
 * User: luft
 * Date: 5/4/13
 * Time: 12:29 PM
 */
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.math.MyInt

object BigIntTests extends Properties("MyInt") {
  property("long addition") = forAll((b: Long, a: Long) =>
    (MyInt(a) + MyInt(b)).toString == (BigInt(a) + b).toString(2)
  )

  property("big addition") = forAll((a: Long, times: Short) => {

    val t = math.abs(times)
    (((0 to t) foldLeft(MyInt(a))) {case (a,b) => a+a} toString) ==
      (((0 to t) foldLeft (BigInt(a))) {case (a,b) => a+a} toString(2))
  })

  property("long multiplication") = forAll((a: Long, b: Long) => {
    (MyInt(a) * MyInt(b)).toString == (BigInt(a) * BigInt(b)).toString(2)
  })

  property("karatsuba multiplication") = forAll((a: Long, b: Long) => {
    val bigOneM = MyInt(10) ** (500 - 1)
    val myInt = (bigOneM * MyInt(a)) * (bigOneM * MyInt(b))
    val bigOneT = BigInt(10).pow(500 - 1)
    val bigInt = (bigOneT * a) * (bigOneT * b)

    myInt.toString == bigInt.toString(2)
  })

  property("big multiplication") = forAll((a: Long, b: Long) => {
    val x = (MyInt(Long.MaxValue) ** 3) * a
    val y = (MyInt(Long.MaxValue) ** 3) * b
    val t = BigInt(Long.MaxValue).pow(3) * a
    val u = BigInt(Long.MaxValue).pow(3) * b

    (x * y).toString == (t * u).toString(2)
  })

  property("pow test") = forAll((a: Byte) => {
    val r = if(a == 0) 1 else if(a < 0) math.abs(a) else a
    (MyInt(10) ** r).toString == BigInt(10).pow(r).toString(2)
  })

  property("bitwise unsigned lessthan") = forAll((a: Long, b: Long) => {
    def ultOld(a: Long, b: Long) = if((a ^ b) < 0) (a >>> 1) < (b >>> 1) else a < b
    val topBit = Long.MinValue
    def bitwiseULT(a: Long, b: Long) = { if((a + topBit) < (b + topBit)) 1 else 0
    }

    bitwiseULT(a,b) == (if(ultOld(a,b)) 1 else 0)
  })

  property("subtraction test") = forAll((a: Long, b: Long, a_exp: Long, b_exp: Long) => {
    val myInt = (MyInt(a) * MyInt(a_exp)) - (MyInt(b) * MyInt(b_exp))
    val bigInt = (BigInt(a) * a_exp) - (BigInt(b) * b_exp)

    myInt.toString == bigInt.toString(2)
  })

  /*property("long multiplication") = forAll((a: Long, b: Long) =>
    (org.math.BigInt.valueOf(a) * org.math.BigInt.valueOf(b)).toString == (BigInt(a) * b).toString(2)
  )

  property("karatsuba multiplication") = forAll((a:Long, b:Long) => {
    val x = (org.math.BigInt.valueOf(Long.MaxValue).^(25) *
      org.math.BigInt.valueOf(a) * org.math.BigInt.valueOf(Long.MaxValue).^(25) *
      org.math.BigInt.valueOf(b)).toString
    val y = (BigInt(Long.MaxValue).pow(25) * a * BigInt(Long.MaxValue).pow(25) * b).toString(2)
    x == y
  })

  property("pow") = forAll((l: Long) => {
    val n = 2
    (org.math.BigInt.valueOf(l) ^ n).toString == (BigInt(l).pow(n).toString(2))
  })*/

  property("shiftleft") = forAll((a: Long, n: Byte) =>
    (MyInt(a) << math.abs(n)).toString == (BigInt(a) << Math.abs(n)).toString(2)
  )

  property("new long mul") = forAll((a: Long, b: Long, c: Long) => {
    MyInt.oldLongMul(a,b,c) == MyInt.properLongMul(a,b,c)
  })

  property("exponentiation") = forAll((a: Long, n: Short) => {
    (MyInt(a) ** math.abs(n % 1024)).toString == BigInt(a).pow(math.abs(n % 1024)).toString(2)
  })

  /*property("big addition") = forAll((times: Short) => {
    val v = Long.MaxValue
    ((0 until times) foldLeft (a,b)) { case ((x,y), l) =>
      (x + org.math.BigInt.valueOf(v)) -> (y + v)
    } match {
      case (x,y) => x.toString == y.toString(2)
    }
  })*/

  //property("multiplication")


}
