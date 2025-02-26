class BlocksForWallTestSuite extends munit.FunSuite:
  test("blocksForWall"):
    assertEquals(blocksForWall(List(0)), 0)
    assertEquals(blocksForWall(List(1)), 1)

    assertEquals(blocksForWall(List(1,1)), 1)
    assertEquals(blocksForWall(List(1,2)), 2)
    assertEquals(blocksForWall(List(2,1)), 2)

    assertEquals(blocksForWall(List(1,2,1)), 2)
    assertEquals(blocksForWall(List(1,2,1)), 2)
    assertEquals(blocksForWall(List(1,2,4)), 3)

    assertEquals(blocksForWall(List(1,2,4,1)), 3)
    assertEquals(blocksForWall(List(1,2,4,2)), 3)
    assertEquals(blocksForWall(List(1,2,4,3)), 4)
    assertEquals(blocksForWall(List(1,2,4,3)), 4)

    // cannot use a block if we have gone below its level
    assertEquals(blocksForWall(List(2,4,1)), 3)
    assertEquals(blocksForWall(List(2,4,1,4)), 4)
    assertEquals(blocksForWall(List(2,4,1,4,2)), 5)
