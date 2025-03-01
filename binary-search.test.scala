import munit.ScalaCheckSuite

class BinarySearchTestSuite extends munit.FunSuite:
    test("binarySearchLeq"):
        assertEquals(binarySearchLeqVector(0, Vector(0)), Some((0, 0)))
        assertEquals(binarySearchLeqVector(1, Vector(0)), Some((0, 0)))
        assertEquals(binarySearchLeqVector(-1, Vector(0)), Some((0, 0)))

        assertEquals(binarySearchLeqVector(0, Vector(0,2)), Some((0, 0)))
        assertEquals(binarySearchLeqVector(1, Vector(0,2)), Some((0, 0)))
        assertEquals(binarySearchLeqVector(2, Vector(0,2)), Some((2, 1)))
        assertEquals(binarySearchLeqVector(3, Vector(0,2)), Some((2, 1)))

    test("squareRoot"):
        assertEquals(squareRoot(0), 0)
        assertEquals(squareRoot(1), 1)
        assertEquals(squareRoot(2), 1)
        assertEquals(squareRoot(3), 1)
        assertEquals(squareRoot(4), 2)
        assertEquals(squareRoot(5), 2)
        assertEquals(squareRoot(6), 2)
        assertEquals(squareRoot(7), 2)
        assertEquals(squareRoot(8), 2)
        assertEquals(squareRoot(9), 3)
        assertEquals(squareRoot(10), 3)
        assertEquals(squareRoot(11), 3)
        assertEquals(squareRoot(12), 3)
        assertEquals(squareRoot(13), 3)
        assertEquals(squareRoot(14), 3)