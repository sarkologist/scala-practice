import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Shrink
import scala.util.Random

case class Majority[A](list: List[A], majority: A)

implicit def shrinkMajority[A]: Shrink[Majority[A]] = Shrink { m =>
  m.list match {
      case h :: t if t != Nil && h == m.majority => 
        // drop one other in addition to majority, to maintain majority status
        val (keep, dropOne) = t.span(_ == m.majority)
        Stream(Majority(keep ++ dropOne.drop(1), m.majority))
      case h :: t if t != Nil => Stream(Majority(t, m.majority))
      case _ => Stream.empty
    }
  }

class MostFrequentTestSuite extends munit.FunSuite with ScalaCheckSuite:
    test("majorityElement"):
        assertEquals(majorityElement(List(0)), Some(0))
        assertEquals(majorityElement(List(0,1)), None)
        assertEquals(majorityElement(List(0,1,1)), Some(1))
        assertEquals(majorityElement(List(0,0,1)), Some(0))
        assertEquals(majorityElement(List(0,0,1,2)), None)
        assertEquals(majorityElement(List(0,0,1,2,3)), None)
    
    def genListWithMajority[A](elem: Gen[A]): Gen[Majority[A]] =
      for {
        listLength <- Gen.choose(1, 100)
        majority <- elem
        excess <- Gen.choose(0, listLength/2)
        halfLengthRoundingUp = (listLength + 1) / 2
        nonMajorities <- Gen.listOfN(Math.max(0, halfLengthRoundingUp - 1 - excess), elem)
        result = Random.shuffle(List.fill(listLength/2 + 1 + excess)(majority) ++ nonMajorities)
      } yield Majority(result, majority)

    property("majorityElement is majority element, when it exists"):
        forAll(genListWithMajority(Gen.choose(1,100))) { case Majority(l,m) =>
            majorityElement(l)
               == Some(m)
        }