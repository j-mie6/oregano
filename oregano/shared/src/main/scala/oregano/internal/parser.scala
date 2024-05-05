package oregano.internal

import parsley.quick.*
import parsley.expr.*
import parsley.syntax.character.*

private object parsers {
    lazy val regex = expr <~ eof
    private lazy val expr = chain.right1(term)(Regex.Alt from '|')
    private lazy val term = Regex.Cat(some(lit))
    private lazy val lit = Regex.Lit(noneOf('[', ']', '|', '(', ')', '.')) // TODO: more
}

private def parse(str: String) = parsers.regex.parse(str).toEither
