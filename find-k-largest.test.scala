import munit.ScalaCheckSuite

class FindKLargestTestSuite extends munit.FunSuite:
    test("findKLargest"):
        assertEquals(findKLargest(List(3,1,2),0), Set())
        assertEquals(findKLargest(List(3,1,2),1), Set(3))
        assertEquals(findKLargest(List(3,1,2),2), Set(3,2))
        assertEquals(findKLargest(List(3,1,2),3), Set(3,2,1))
        assertEquals(findKLargest(List(3,1,2),4), Set(3,2,1))
