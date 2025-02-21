def rotate(a: Array[Int], k: Int): Array[Int] =
    if (a.isEmpty) then
        a 
    else if (k < 0) then
        rotate(a, a.length + (k % a.length))
    else
        val r = a.length - (k % a.length)

        (a.view ++ a.view)
        .drop(r)
        .take(a.length)
        .toArray