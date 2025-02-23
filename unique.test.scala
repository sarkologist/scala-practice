import munit.ScalaCheckSuite

class UniqueTestSuite extends munit.FunSuite:
    test("onlyUniqueElements"):
        assertEquals(onlyUniqueElements(List()), Set())
        assertEquals(onlyUniqueElements(List(1,1,2,3,3,3)), Set(2))
        assertEquals(onlyUniqueElements(List(1,1,2,2,3,3,3)), Set())