def onlyUniqueElements[A](xs: Iterable[A]): Set[A] =
    xs.foldLeft(Set.empty[A], Set.empty[A]){ (acc, x) =>
        acc match 
            case (more, once) =>
                if once.contains(x) || more.contains(x) then
                    (more + x, once - x)
                else (more, once + x)
        
    }._2

def isPermutation[A](xs: Seq[A]): Boolean =
    xs.toSet.size == xs.length

def isPermutationOfRange(a: Seq[Int]): Boolean = 
    val remaining = a.foldLeft(Range(1,a.length+1).toSet) { case (acc, x) =>
      acc - x
    }

    remaining.isEmpty