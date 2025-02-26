import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen
import scala.util.Random

def shuffledRangeGen: Gen[List[Int]] = for {
    n <- Gen.choose(0, 100)
} yield Random.shuffle((1 to n).toList)

class UniqueTestSuite extends munit.FunSuite with ScalaCheckSuite:
    test("onlyUniqueElements"):
        assertEquals(onlyUniqueElements(List()), Set())
        assertEquals(onlyUniqueElements(List(1,1,2,3,3,3)), Set(2))
        assertEquals(onlyUniqueElements(List(1,1,2,2,3,3,3)), Set())

    test("isPermutationOfRange"):
        assert(isPermutationOfRange(Seq()))
        assert(isPermutationOfRange(Seq(1)))
        assert(isPermutationOfRange(Seq(2,1)))
        assert(!isPermutationOfRange(Seq(3,1)))

    property("permutation of range is permutation"):
        forAll(shuffledRangeGen): (x) =>
            assert(isPermutation(x) == isPermutationOfRange(x))