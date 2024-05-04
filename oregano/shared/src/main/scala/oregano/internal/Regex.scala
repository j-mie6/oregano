package oregano.internal

import parsley.generic.*

private enum Regex {
    case Lit(c: Char)
    case Cat(rs: List[Regex])
    case Alt(r1: Regex, r2: Regex)
    case Class(inc: Boolean, cs: Set[Char])
    case Capture(r: Regex)
}
private object Regex {
    object Lit extends ParserBridge1[Char, Regex]
    object Cat extends ParserBridge1[List[Regex], Regex]
    object Alt extends ParserBridge2[Regex, Regex, Regex]
    object Class extends ParserBridge2[Boolean, Set[Char], Regex]
    object Capture extends ParserBridge1[Regex, Regex]
}
