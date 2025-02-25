import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen

class KAnagramSuite extends munit.FunSuite with ScalaCheckSuite:
  test("replacementsNeeded should be correct") {
    assertEquals(replacementsNeeded("", ""), Some(0))
    assertEquals(replacementsNeeded("x", ""), None)
    assertEquals(replacementsNeeded("x", "y"), Some(1))
    assertEquals(replacementsNeeded("xa", "ya"), Some(1))
    assertEquals(replacementsNeeded("xa", "yb"), Some(2))
    assertEquals(replacementsNeeded("anagram", "grammar"), Some(2))
  }

  // Generator for pairs of strings
  val stringPairGen: Gen[(String, String)] = for {
    s1 <- Gen.alphaStr
    s2 <- Gen.alphaStr
  } yield (s1, s2)

  test("replacementsNeeded is symmetric"):
    forAll(stringPairGen) { case (x, y) =>
      assert(replacementsNeeded(x, y) == replacementsNeeded(y, x))
    }
  

  test("isKAnagram is equal to anagram for k=0"):
    forAll(stringPairGen) { case (x, y) =>
      isAnagramSimple(x, y) == isKAnagram(0)(x, y)
    }
  
