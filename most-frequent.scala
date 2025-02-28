// returns None if the input list is empty
// makes 2 passes over the list
def majorityElement[A](fa: Iterable[A]): Option[A] = 
  val f = (acc: (Int, Option[A]), x: A) => acc match {
    case (n, candidate) =>
      // new candidate
      if (n == 0) {
        (n + 1, Some(x))
      // if there are as many other elements as candidate, then we can discard candidate
      } else if (candidate != Some(x)) {
        (n - 1, candidate)
      // one more occurence of candidate
      } else { // candidate == Some(x)
        (n + 1, candidate)
      }
  }
  
  // first pass: identify candidate
  val candidate = fa.foldLeft((0, None: Option[A]))(f)._2
  
  // second pass: check candidate is indeed majority
  fa.foldLeft(0)((acc, x) => if (Some(x) == candidate) acc + 1 else acc) match {
    case n if n > fa.size / 2 => candidate
    case _ => None
  }
