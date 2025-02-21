def rotate(a: Array[Int], k: Int): Array[Int] = {
    if (a.isEmpty) a 
    else if (k < 0) rotate(a, a.length + (k % a.length))
    else {
        val r = a.length - (k % a.length)

        (a.view ++ a.view)
        .drop(r)
        .take(a.length)
        .toArray
    }
}