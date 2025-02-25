import scala.collection.immutable.HashMap
import scala.util.hashing.Hashing

trait Semigroup[A]:
  def combine(x: A, y: A): A
  extension (x: A)
    def <>(y: A): A = combine(x, y)

trait Monoid[A] extends Semigroup[A]:
  def empty: A

trait Group[A] extends Monoid[A]:
  def invert(a: A): A
  extension (a: A)
    def inverse: A = invert(a)

given [A](using num: Numeric[A]): Group[A] with
  def empty: A = num.zero
  def combine(x: A, y: A): A = num.plus(x, y)
  def invert(a: A): A = num.negate(a)

case class MergeMap[K, V](map: HashMap[K, V]):
  override def toString: String = s"MergeMap($map)"
  override def equals(obj: Any): Boolean = 
    obj match
      case MergeMap(otherMap) => map == otherMap
      case _ => false

given [K: Hashing, V: Semigroup]: Semigroup[MergeMap[K, V]] with
  def combine(mx: MergeMap[K, V], my: MergeMap[K, V]): MergeMap[K, V] =
    MergeMap((mx,my) match
      case (MergeMap(x), MergeMap(y)) => x.merged(y) {
         case ((k, v1), (_, v2)) => (k, v1 <> v2)
       }
    )

def isKAnagram(k: Int)(a: String, b: String): Boolean =
  replacementsNeeded(a, b).exists(_ <= k)

def replacementsNeeded(a: String, b: String)
    (using Group[Int])
    : Option[Int] =
  unmatchedCount(a, b).map { mergeMap =>
    val count = mergeMap.map.values
      .map(Math.abs)
      .foldLeft(0)(_ <> _)
    val (q, r) = (count / 2, count % 2)
    if r == 0 then Some(q)
    else throw new Error("replacementsNeeded: unmatchedCount should have returned" +
      " pairs of counts with opposite sign but equal value")
  }.flatten

def unmatchedCount(a: String, b: String)
    (using Semigroup[MergeMap[Char, Int]], Group[Int])
    : Option[MergeMap[Char, Int]] =
  if a.length != b.length then None
  else
    var acc = MergeMap(HashMap.empty[Char, Int])
    for i <- 0 until a.length do
      acc = acc <>
        MergeMap(HashMap(a.charAt(i) -> 1)) <>
        MergeMap(HashMap(b.charAt(i) -> 1.inverse))
    Some(acc)


def isAnagramSimple(x: String, y: String): Boolean =
    (x.length == y.length) &&
     (letterFrequenciesSimple(x) == letterFrequenciesSimple(y))

def letterFrequenciesSimple(s: String): HashMap[Char, Long] =
    s.foldLeft(HashMap.empty[Char, Long]) { (acc, char) =>
      val lowerChar = char.toLower
      acc.updatedWith(lowerChar) {
        case None => Some(1L)
        case Some(count) => Some(count + 1)
      }
    }