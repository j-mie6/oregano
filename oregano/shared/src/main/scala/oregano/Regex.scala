package oregano

import scala.quoted.*

abstract class Regex[Match] {
    def matches(input: String): Boolean
    def unapplySeq(input: String): Match
}

// FIXME: wrong type, not sure how I want to process the typesafe bit yet, ideally avoid duplication, but might have to :(
inline def compile[S <: String & Singleton](inline regex: S): Regex[S] = ${compileMacro('regex)}

// this, annoyingly, has to be here or else the splice above complains that it's in a different scope
private [oregano] def compileMacro[S <: String & Singleton: Type](s: Expr[S])(using Quotes): Expr[Regex[S]] = internal.compileMacro[S](s.valueOrAbort)
