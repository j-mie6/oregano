package test

import oregano.compile

@main def test() = {
    lazy val reg = compile("abc|def")
}
