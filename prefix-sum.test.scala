import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen

class PrefixSumsTestSuite extends munit.FunSuite with ScalaCheckSuite:
    test("sumOfSlice"):
        assertEquals(sumOfSlice(Array(1,2,3,4), 0, 0), 1)
        assertEquals(sumOfSlice(Array(1,2,3,4), 0, 1), 3)
        assertEquals(sumOfSlice(Array(1,2,3,4), 1, 1), 2)
        assertEquals(sumOfSlice(Array(1,2,3,4), 1, 2), 5)

    val genSeqWithIndices: Gen[(IndexedSeq[Int], Int, Int)] = 
        Gen.nonEmptyContainerOf[IndexedSeq, Int](Gen.chooseNum(1,100))
          .flatMap { seq =>
            val maxIndex = seq.length - 1
            for {
                x <- Gen.choose(0, maxIndex)
                y <- Gen.choose(x, maxIndex)
            } yield (seq, x, y)
        }

    property("sumOfSlice is sum of slice"):
        forAll(genSeqWithIndices) { case (p, x, y) =>
            sumOfSlice(p, x, y) == p.slice(x, y + 1).sum
        }
    property("sumOfSlice of empty range is element at index"):
        forAll(genSeqWithIndices) { case (p, x, _) =>
            sumOfSlice(p, x, x) == p(x)
        }