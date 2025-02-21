//> using dep org.scalameta::munit:1.1.0

class TestSuite extends munit.FunSuite {
  test("rotate") {
    assertEquals(rotate(Array(1,2,3), 1).toSeq, Array(3,1,2).toSeq)
    assertEquals(rotate(Array(1,2,3), 2).toSeq, Array(2,3,1).toSeq)
    assertEquals(rotate(Array(1,2,3), 3).toSeq, Array(1,2,3).toSeq)
  }
}