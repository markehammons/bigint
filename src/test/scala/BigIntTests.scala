/**
 * Created with IntelliJ IDEA.
 * User: luft
 * Date: 5/4/13
 * Time: 12:29 PM
 */
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.math.MyInt

object BigIntTests extends Properties("New BigInt") {
  property("long addition") = forAll((b: Long, a: Long) =>
    (MyInt(a) + MyInt(b)).toString == (BigInt(a) + b).toString(2)
  )

  property("big addition") = forAll((a: Long, times: Short) => {

    val t = math.abs(times)
    (((0 to t) foldLeft(MyInt(a))) {case (a,b) => a+a} toString) ==
      (((0 to t) foldLeft (BigInt(a))) {case (a,b) => a+a} toString(2))
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
    if(n >= 0) org.math.BigInt.valueOf(a).shiftLeft(n*32).toString == (BigInt(a) << n*32).toString(2) else true
  )

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
