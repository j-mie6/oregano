package test

import oregano.regex

@main def test() = {
    val reg = regex("abc|d.e")
    println(reg.matches("abc"))
}
