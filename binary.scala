def binaryDigits(n: Int): List[Int] = 
    def go(n: Int, acc: List[Int]): List[Int] = 
        if n == 0 then acc
        else go(n / 2, n % 2 :: acc)
    if n == 0 then List(0)
    else go(n, List.empty)
