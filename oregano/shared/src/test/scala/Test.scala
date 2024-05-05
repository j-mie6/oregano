package test

import oregano.regex

@main def test() = {
    val reg = regex("abc|de")
    println(reg.matches("abc"))
}
