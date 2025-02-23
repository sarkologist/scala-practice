import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class BinarySearchTestSuite extends munit.FunSuite with ScalaCheckSuite:
  test("binarySearchLeq"):
    assertEquals(binarySearchLeq(0, Vector(0)), Some((0, 0)))
    assertEquals(binarySearchLeq(1, Vector(0)), Some((0, 0)))
    assertEquals(binarySearchLeq(-1, Vector(0)), Some((0, 0)))

    assertEquals(binarySearchLeq(0, Vector(0,2)), Some((0, 0)))
    assertEquals(binarySearchLeq(1, Vector(0,2)), Some((0, 0)))
    assertEquals(binarySearchLeq(2, Vector(0,2)), Some((2, 1)))
    assertEquals(binarySearchLeq(3, Vector(0,2)), Some((2, 1)))