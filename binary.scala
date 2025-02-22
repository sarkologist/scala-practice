def binaryDigits(n: Int): List[Int] = 
    if n == 0 then List(0)
    else Iterator
        .iterate(n)(_ / 2)
        .takeWhile(_ != 0)
        .map(_ % 2)
        .toList
        .reverse
