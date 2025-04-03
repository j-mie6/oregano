/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import scala.quoted.*
import oregano.internal.Pattern.matchPattern

private [oregano] def compileMacro(s: String)(using Quotes): Expr[oregano.Regex[?]] = {
    import quotes.reflect.report
    parse(s) match
    case Right(ast) =>
        // report.info(s"$ast")
        val p = Pattern.compile(ast)
        report.info(s"Parsley AST: ${ast.toString}\nPattern: $p")
        val prog = ProgramCompiler.compileRegexp(p)
        '{
            // just do compile-time checks for now!
            new oregano.Regex[List[String]] {
                val liftedProg = ${Expr(prog)} // in practice we want this to be in an object
                val regex = ${Expr(s)}.r
                // def matches(input: CharSequence): Boolean = regex.matches(input)
                def matches(input: CharSequence): Boolean = {
                    val endPos: Int = ${ matchPattern(p, '{input}, '{0}) }
                    return endPos == input.length
                }
                def unapplySeq(input: CharSequence): Option[List[String]] = regex.unapplySeq(input)
            }
        }
    case Left(err) => report.errorAndAbort(err)
}
