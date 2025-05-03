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
        report.info(s"Parsley AST: ${ast.toString}\nPattern: $p")
        val prog = ProgramCompiler.compileRegexp(p)
        report.info(s"Prog:\n $prog")
        val liftedProgExpr = Expr(prog)
        val backtrackMatcherExpr = VMCodegen.genMatcher(prog)
        // val linearMatcherExpr = VMCodegenLinear.genMatcherRE2(prog)
        val flatTableExpr = FlatTable.build(prog)
        val linearMatcherExpr = VMCodegenFlat.genMatcherFlat(prog)
        // quotes.reflect.report.info(matcherExpr.show)
        '{
            // just do compile-time checks for now!
            new oregano.Regex[List[String]] {
                val regex = ${Expr(s)}.r
                val prog = $liftedProgExpr
                val flatTable = $flatTableExpr
                def matches(input: CharSequence): Boolean = regex.matches(input)
                def matchesRuntime(input: CharSequence): Boolean = Matcher.matches(prog, input)
                def matchesLinear(input: CharSequence): Boolean = $linearMatcherExpr(input, flatTable)
                def matchesBacktrack(input: CharSequence): Boolean = $backtrackMatcherExpr(input)
                def unapplySeq(input: CharSequence): Option[List[String]] = regex.unapplySeq(input)
            }
        }
    case Left(err) => report.errorAndAbort(err)
}

inline def testMatch: Int => Boolean = ${ testMatchImpl }

def testMatchImpl(using Quotes): Expr[Int => Boolean] =
  val inst = Inst(InstOp.RUNE, 0, 0, Array(97, 97, 98, 98, 99, 99, 100, 100, 101, 101)) // "(abde)"
  val expr = inst.matchRuneExpr 
  // quotes.reflect.report.info(expr.show)
  expr