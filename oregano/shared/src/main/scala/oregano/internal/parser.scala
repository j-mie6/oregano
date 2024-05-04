package oregano.internal

import parsley.quick.*
import parsley.expr.*
import parsley.syntax.character.*

private object parsers {
    lazy val regex = chain.right1(term)(Regex.Alt from '|')
    lazy val term = Regex.Cat(some(lit))
    lazy val lit = Regex.Lit(noneOf('[', ']', '|', '(', ')')) // TODO: more
}

private def parse(str: String): Option[Regex] = parsers.regex.parse(str).toOption
