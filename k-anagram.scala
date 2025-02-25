import scala.collection.immutable.HashMap
import scala.util.hashing.Hashing

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]
  extension [A](fa: F[A])
    def fmap[B](f: A => B): F[B] = map(fa)(f)

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

case class Sum[A](get: A)

given Functor[Sum] with
  def map[A, B](fa: Sum[A])(f: A => B): Sum[B] = Sum(f(fa.get))

given [A](using num: Numeric[A]): Group[Sum[A]] with
  def empty: Sum[A] = Sum(num.zero)
  def combine(x: Sum[A], y: Sum[A]): Sum[A] = Sum(num.plus(x.get, y.get))
  def invert(a: Sum[A]): Sum[A] = Sum(num.negate(a.get))

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
    (using Functor[Sum], Group[Sum[Int]])
    : Option[Int] =
  unmatchedCount(a, b).map { mergeMap =>
    val Sum(count) = mergeMap.map.values
      .map(_.fmap(Math.abs))
      .foldLeft(Sum(0))(_ <> _)
    val (q, r) = (count / 2, count % 2)
    if r == 0 then Some(q)
    else throw new Error("replacementsNeeded: unmatchedCount should have returned" +
      " pairs of counts with opposite sign but equal value")
  }.flatten

def unmatchedCount(a: String, b: String)
    (using Semigroup[MergeMap[Char, Sum[Int]]], Group[Sum[Int]])
    : Option[MergeMap[Char, Sum[Int]]] =
  if a.length != b.length then None
  else
    var acc = MergeMap(HashMap.empty[Char, Sum[Int]])
    for i <- 0 until a.length do
      acc = acc <>
        MergeMap(HashMap(a.charAt(i) -> Sum(1))) <>
        MergeMap(HashMap(b.charAt(i) -> Sum(1).inverse))
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