/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano

import scala.quoted.*

abstract class Regex[Match] {
  def matches(input: CharSequence): Boolean
  def unapplySeq(input: CharSequence): Option[Match]
}

object Regex {
  // Fallback method for runtime regexes
  def runtime(s: String): Regex[?] = new Regex[List[String]] {
    private val compiled = s.r
    // private val pattern = internal.Pattern.compile(internal.parse(s).)
    // def matches(input: CharSequence): Boolean = compiled.matches(input)
    def matches(input: CharSequence): Boolean = {
      internal.Pattern.compile(s).matches(input)
    }
    def unapplySeq(input: CharSequence): Option[List[String]] = compiled.unapplySeq(input)
  }
}

extension (inline r: String)
  inline def regex: Regex[?] =
    ${ isInlineable('r) }

private def isInlineable(regExpr: Expr[String])(using Quotes): Expr[Regex[?]] = {
  regExpr match {
    case Expr(s) =>
      // use the macro, inlineable
      internal.compileMacro(s)
    case _ =>
      // fallback to runtime compilation
      '{ Regex.runtime($regExpr) }
  }
}

// FIXME: wrong type, not sure how I want to process the typesafe bit yet, ideally avoid duplication, but might have to :(
//transparent inline def regex/*[S <: String & Singleton]*/(inline regex: String): Regex[?] = ${compileMacro('regex)}

// this, annoyingly, has to be here or else the splice above complains that it's in a different scope
private def compileMacro(s: Expr[String])(using Quotes): Expr[Regex[?]] =
  internal.compileMacro(s.valueOrAbort)
