// we can extend a block if the height is the same
// if the height is lower, add a new block,
//   and the previous block is no longer useful
//   in fact we have to keep popping previous blocks until we are below the needed height
// if the height is higher, we can add a new block on top of the previous one
def blocksForWall(h: Seq[Int]): Int = 
  // walk through the height required at each position of the wall
  h.foldLeft(List.empty[Int], 0) {
    case ((stack, blocks), height) =>
    // determine if a new block is needed
    // adjust the stack accordingly
    Iterator.iterate(Left(stack): Either[List[Int], (List[Int], Int)]) {
        // height is the same, no need for new block
        case Left(next :: rest) if height == next => Right((  next :: rest        , blocks))
        // need to be taller, add a block to the stack
        case Left(next :: rest) if height > next  => Right((height :: next :: rest, blocks + 1))
        // pop blocks until we are below the needed height
        case Left(next :: rest) if height < next  => Left(rest)
        // if there are no blocks, and a new one equal to the height needed
        case Left(emptyStack) if height > 0       => Right((height :: emptyStack  , blocks + 1))
        case Left(emptyStack)                     => Right((emptyStack  , blocks))
        // done
        case Right(x)                             => Right(x)
    }.dropWhile(_.isLeft).next.getOrElse(throw new RuntimeException("match error in unwinding stack"))
}._2
