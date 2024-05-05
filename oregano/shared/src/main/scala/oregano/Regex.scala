package oregano

import scala.quoted.*

abstract class Regex[Match] {
    def matches(input: String): Boolean
    def unapplySeq(input: String): Match
}

// FIXME: wrong type, not sure how I want to process the typesafe bit yet, ideally avoid duplication, but might have to :(
transparent inline def compile/*[S <: String & Singleton]*/(inline regex: String): Regex[?] = ${compileMacro('regex)}

// this, annoyingly, has to be here or else the splice above complains that it's in a different scope
private def compileMacro(s: Expr[String])(using Quotes): Expr[Regex[?]] = internal.compileMacro(s.valueOrAbort)
