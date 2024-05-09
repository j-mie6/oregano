/*
 * Copyright 2022 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano

import scala.quoted.*

abstract class Regex[Match] {
    def matches(input: CharSequence): Boolean
    def unapplySeq(input: CharSequence): Option[Match]
}

extension (inline r: String)
    transparent inline def regex: Regex[?] = ${compileMacro('r)}

// FIXME: wrong type, not sure how I want to process the typesafe bit yet, ideally avoid duplication, but might have to :(
//transparent inline def regex/*[S <: String & Singleton]*/(inline regex: String): Regex[?] = ${compileMacro('regex)}

// this, annoyingly, has to be here or else the splice above complains that it's in a different scope
private def compileMacro(s: Expr[String])(using Quotes): Expr[Regex[?]] = internal.compileMacro(s.valueOrAbort)
