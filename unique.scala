def onlyUniqueElements[A](xs: List[A]): Set[A] =
    val (more, once) = xs.foldLeft(Set.empty[A], Set.empty[A]){ (acc, x) =>
        acc match 
            case (more, once) =>
                if once.contains(x) then
                    (more + x, once)
                else (more, once + x)
        
    }

    once -- more