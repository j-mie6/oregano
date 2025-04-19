/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import scala.quoted.*
// import oregano.internal.Pattern.matchPattern

private [oregano] def compileMacro(s: String)(using Quotes): Expr[oregano.Regex[?]] = {
    import quotes.reflect.report
    parse(s) match
    case Right(ast) =>
        // report.info(s"$ast")
        val p = Pattern.compile(ast)
        // report.info(s"Parsley AST: ${ast.toString}\nPattern: $p")
        val prog = ProgramCompiler.compileRegexp(p)
        // val liftedProgExpr = Expr(prog)
        // val matcherExpr = VMCodegen.genMatcher(prog)
        // val matcherExpr = VMCodegenCPS.genMatcher(prog)
        val matcherExpr = VMCodegenLinear.genMatcher(prog)
        quotes.reflect.report.info(matcherExpr.show)
        '{
            // just do compile-time checks for now!
            new oregano.Regex[List[String]] {
                val regex = ${Expr(s)}.r
                // def matches(input: CharSequence): Boolean = regex.matches(input)
                def matches(input: CharSequence): Boolean = $matcherExpr(input)
                def unapplySeq(input: CharSequence): Option[List[String]] = regex.unapplySeq(input)
            }
        }
    case Left(err) => report.errorAndAbort(err)
}

inline def testMatch: Int => Boolean = ${ testMatchImpl }

def testMatchImpl(using Quotes): Expr[Int => Boolean] =
  val inst = Inst(InstOp.RUNE, 0, 0, Array(97, 98, 100, 101)) // "(abde)"
  val expr = inst.matchRuneExpr 
  quotes.reflect.report.info(expr.show)
  expr