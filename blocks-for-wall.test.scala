import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class BlocksForWallTestSuite extends munit.FunSuite with ScalaCheckSuite:
  test("blocksForWall"):
    assertEquals(blocksForWall(List(0)), 0)
    assertEquals(blocksForWall(List(1)), 1)

    // need a new block if height is different
    assertEquals(blocksForWall(List(1,1)), 1)
    assertEquals(blocksForWall(List(1,2)), 2)
    assertEquals(blocksForWall(List(2,1)), 2)

    // can extent a previous block if we drop to the same level as it
    assertEquals(blocksForWall(List(1,2,1)), 2)
    assertEquals(blocksForWall(List(1,2,1)), 2)
    assertEquals(blocksForWall(List(1,2,4)), 3)

    // cannot extend a previous block if we don't drop to the same level as it
    assertEquals(blocksForWall(List(1,2,4,1)), 3)
    assertEquals(blocksForWall(List(1,2,4,2)), 3)
    assertEquals(blocksForWall(List(1,2,4,3)), 4)

    // cannot use a block if we have gone below its level
    assertEquals(blocksForWall(List(2,4,1)), 3)
    assertEquals(blocksForWall(List(2,4,1,4)), 4)
    assertEquals(blocksForWall(List(2,4,1,4,2)), 5)

  property("for permutation: need as many blocks as number of elements"):
        forAll(shuffledRangeGen): (x) =>
            assert(blocksForWall(x) == x.length)