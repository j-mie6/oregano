package oregano.internal

@main def testMatchShenanigans(): Unit =
    val matcher = testMatch
    println(matcher(97)) 
    println(matcher(98)) 
    println(matcher(99)) 
    println(matcher(100)) 
    println(matcher(101)) 