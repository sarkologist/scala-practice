def binarySearchLeq[A](target: A, xs: Vector[A])(implicit ord: Ordering[A]): Option[(A, Int)] = 
    if (xs.isEmpty) then None
    else
        def go(start: Int, end: Int): Option[(A, Int)] = 
            import ord.mkOrderingOps

            if (start <= end) then
                val mid = (start + end) / 2
                xs(mid) match {
                    case x if x == target => Some((x, mid))
                    case x if x < target => go(mid + 1, end)
                    case x if x > target => go(start, mid - 1)
                    case _ => throw new RuntimeException("unreachable: violates trichotomy")
                }
            else Some((xs(Math.max(0, end)), Math.max(0, end)))

        go(0, xs.length - 1)
