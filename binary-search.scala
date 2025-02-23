def binarySearchLeqVector[A](target: A, xs: Vector[A])(implicit ord: Ordering[A]): Option[(A, Int)] = 
    if (xs.isEmpty) then None
    else Some(binarySearchLeq(target, xs(_), 0, xs.size - 1))

def binarySearchLeq[A](target: A, index: Int => A, left: Int, right: Int)(implicit ord: Ordering[A]): (A, Int) = 
    def go(start: Int, end: Int): (A, Int) = 
        import ord.mkOrderingOps

        if (start <= end) then
            val mid = (start + end) / 2
            index(mid) match {
                case x if x == target => (x, mid)
                case x if x < target => go(mid + 1, end)
                case x if x > target => go(start, mid - 1)
                case _ => throw new RuntimeException("unreachable: violates trichotomy")
            }
        else (index(Math.max(left, end)), Math.max(left, end))

    go(left, right)

def squareRoot(n: Int): Int =
    n match {
        case 0 => 0
        case 1 => 1
        case _ => binarySearchLeq(n, (x) => x*x, 0, n / 2)._2
    }

