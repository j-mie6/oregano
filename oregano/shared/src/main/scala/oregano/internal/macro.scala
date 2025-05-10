/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import scala.quoted.*
import codes.quine.labo.re2s._

private [oregano] def compileMacro(s: String)(using Quotes): Expr[oregano.Regex[?]] = {
    import quotes.reflect.report
    parse(s) match
    case Right(ast) =>
        // report.info(s"$ast")
        val (p, groupCount) = Pattern.compile(ast)
        report.info(s"Parsley AST: ${ast.toString}\nPattern: $p")
        val prog = ProgramCompiler.compileRegexp(p, groupCount)
        // // report.info(s"Prog:\n$prog")
        val liftedProgExpr = Expr(prog)
        val backtrackProgMatcherExpr = BacktrackingProgMatcher.genMatcher(prog)
        // val backtrackCPSMatcherExpr = CPSMatcher.genMatcherPattern(p, groupCount)
        // // report.info(backtrackMatcherExpr.show)
        // val linearMatcherExpr = LinearMatcher.genMatcher(prog)
        // val flatTableExpr = FlatTable.build(prog)
        // val linearMatcherExpr = VMCodegenFlat.genMatcherFlat(prog)
        val machineMatcherExpr = StagedMachine.genMachineMatcher(prog)
        val buildMachineTable = StagedMachine.buildMachineTable(prog)
        // quotes.reflect.report.info(matcherExpr.show)
        // val funcTable = LinearMatcher.buildTable(prog)
        println(s"Prog:\n$prog")
        '{
            new oregano.Regex[List[String]] {
                val regex = ${Expr(s)}.r
                val prog = $liftedProgExpr
                val re2machine = RE2Machine(prog)
                val machineTable = $buildMachineTable
                // re2machine.init(prog.numCap)
                // val q0 = ThreadQueue(prog.numInst)
                // val q1 = ThreadQueue(prog.numInst)
                // val re2Regex = ${Expr(s)}.r2
                // val prog = $liftedProgExpr
                // val flatTable = $flatTableExpr
                // def matches(input: CharSequence): Boolean = regex.matches(input)
                def matches(input: CharSequence): Boolean = re2machine.matches(input)
                // def matchesRuntimeLinear(input: CharSequence): Boolean = MatcherFlat.matches(input, flatTable)
                // // def matchesRuntimeLinear(input: CharSequence): Boolean = MatcherFlatProg.matches(prog, input)
                // // def matchesRuntimeLinear(input: CharSequence): Boolean = {
                // //   re2Regex.matches(input)
                // // }   
                // def matchesRuntimeBacktrack(input: CharSequence): Boolean = VMCodegenBacktracking.matches(prog, input)
                def matchesLinear(input: CharSequence): Boolean = $machineMatcherExpr(input, machineTable, re2machine)
                def matchesBacktrack(input: CharSequence): Boolean = $backtrackProgMatcherExpr(input)
                def unapplySeq(input: CharSequence): Option[List[String]] = regex.unapplySeq(input)
            }
        }
    case Left(err) => report.errorAndAbort(err)
}

inline def testMatch: Int => Boolean = ${ testMatchImpl }

def testMatchImpl(using Quotes): Expr[Int => Boolean] =
  val inst = Inst(InstOp.RUNE, 0, 0, IArray(97, 97, 98, 98, 99, 99, 100, 100, 101, 101)) // "(abde)"
  val expr = inst.matchRuneExpr 
  expr