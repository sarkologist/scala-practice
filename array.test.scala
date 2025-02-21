//> using dep org.scalameta::munit:1.1.0
//> using dep org.scalameta::munit-scalacheck:1.1.0
import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class TestSuite extends munit.FunSuite with ScalaCheckSuite:
  test("rotate, negative k"):
    assertEquals(rotate(Array(1,2,3), -2).toSeq, Array(3,1,2).toSeq)
    assertEquals(rotate(Array(1,2,3), -3).toSeq, Array(1,2,3).toSeq)
  

  test("rotate"):
    assertEquals(rotate(Array[Int](), 1).toSeq, Array[Int]().toSeq)
    assertEquals(rotate(Array(1,2,3), 1).toSeq, Array(3,1,2).toSeq)
    assertEquals(rotate(Array(1,2,3), 2).toSeq, Array(2,3,1).toSeq)
    assertEquals(rotate(Array(1,2,3), 3).toSeq, Array(1,2,3).toSeq)
  

  property("rotate by the length of the array returns original array"): 
    forAll: (a: Array[Int]) =>
      assertEquals(rotate(a, a.length).toSeq, a.toSeq)
    
  

  property("rotating by i, then j, is the same as rotating by i+j"):
    forAll: (a: Array[Int], i: Int, j: Int) =>
      // check for integer overflow
      (BigInt(i) + BigInt(j) > BigInt(Int.MinValue) && BigInt(i) + BigInt(j) < BigInt(Int.MaxValue)) ==>
        assertEquals(rotate(rotate(a, i), j).toSeq, rotate(a, i+j).toSeq)
    
  
