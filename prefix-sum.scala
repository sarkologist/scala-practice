def prefixSums(a: IndexedSeq[Int]): Array[Int] = 
    val n = a.length
    val p = Array.fill(n + 1)(0)

    for (i <- 0 until n) 
        p(i + 1) = p(i) + a(i)
    
    p

def sumOfSlice(p: IndexedSeq[Int], x: Int, y: Int): Int = 
    val prefixes = prefixSums(p)
    prefixes(y+1) - prefixes(x)
