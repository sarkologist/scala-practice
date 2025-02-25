class RpnTestSuite extends munit.FunSuite:
    test("rpn"):
        assertEquals(eval("1 2 +"), 3.0)
        assertEquals(eval("2 3 4 + *"), 14.0)