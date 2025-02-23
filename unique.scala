def onlyUniqueElements[A](xs: Iterable[A]): Set[A] =
    xs.foldLeft(Set.empty[A], Set.empty[A]){ (acc, x) =>
        acc match 
            case (more, once) =>
                if once.contains(x) || more.contains(x) then
                    (more + x, once - x)
                else (more, once + x)
        
    }._2