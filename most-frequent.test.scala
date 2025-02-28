import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen

case class Majority[A](list: List[A], majority: A)

class MostFrequentTestSuite extends munit.FunSuite with ScalaCheckSuite:
    test("majorityElement"):
        assertEquals(majorityElement(List(0)), Some(0))
        assertEquals(majorityElement(List(0,1)), None)
        assertEquals(majorityElement(List(0,1,1)), Some(1))
        assertEquals(majorityElement(List(0,0,1)), Some(0))
        assertEquals(majorityElement(List(0,0,1,2)), None)
        assertEquals(majorityElement(List(0,0,1,2,3)), None)
    
    def genListWithMajority[A](elem: Gen[A]): Gen[Majority[A]] =
      def go[A](majority: A)(list: Gen[(List[A], Int, Int)]): Gen[(List[A], Int, Int)] =
        for {
          (l, remainingMajority, remaining) <- list
          addMajority <- Gen.oneOf(true, false)
          nonMajority <- elem // fine if we make it "even more of a majority"
          done = remainingMajority <= 0 && remaining <= 0
          next = if done then (l, 0, 0)
                 else if remaining <= 0 || (addMajority && remainingMajority > 0) then (majority :: l, remainingMajority - 1, remaining)
                 else (nonMajority.asInstanceOf[A] :: l, remainingMajority, remaining - 1)
          result <- if done then Gen.const(next)
                    else go(majority)(Gen.const(next))
        } yield result
                
      for {
        listLength <- Gen.choose(1, 100)
        majority <- elem
        // ensure we have listLength/2 + 1 occurences of majority
        (result, _, _) <- go(majority)(Gen.const((List(majority), listLength/2, listLength/2-1)))
      } yield Majority(result, majority)

    property("majorityElement is majority element, when it exists"):
        forAll(genListWithMajority(Gen.choose(1,100))) { case Majority(l,m) =>
            majorityElement(l)
               == Some(m)
        }