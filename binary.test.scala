import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class BinaryDigitsTestSuite extends munit.FunSuite with ScalaCheckSuite:
  test("binaryDigits"):
    assertEquals(binaryDigits(0), List(0))
    assertEquals(binaryDigits(1), List(1))
    assertEquals(binaryDigits(2), List(1,0))
    assertEquals(binaryDigits(3), List(1,1))
    assertEquals(binaryDigits(4), List(1,0,0))
    assertEquals(binaryDigits(5), List(1,0,1))
  