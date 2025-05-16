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
        val patternResult = Pattern.compile(ast)
        // report.info(s"Parsley AST: ${ast.toString}\nPattern: $p")
        report.info(s"Parsley AST: ${ast.toString}\nPattern: ${patternResult.pattern}, groupCount: ${patternResult.groupCount}")
        val p = patternResult.pattern
        val groupCount = patternResult.groupCount
        val stageable = patternResult.stageable
        val prog = ProgramCompiler.compileRegexp(p, groupCount)
        // // report.info(s"Prog:\n$prog")
        val liftedProgExpr = Expr(prog)
        println(s"Prog:\n$prog")
        // ideally shouldn't compile this if not using!!
        val backtrackProgMatcherExpr: Expr[CharSequence => Boolean] = BacktrackingProgMatcher.genMatcherWithCaps(prog)
          
        val lambdaBacktrackProgMatcherExpr = BacktrackingProgMatcher.genMatcherLambda(prog)
        val backtrackCPSMatcherExpr = CPSMatcher.genMatcherPattern(p)
        // // report.info(backtrackMatcherExpr.show)
        // val linearMatcherExpr = LinearMatcher.genMatcher(prog)
        // val flatTableExpr = LinearMatcher.buildTable(prog)
        // val linearMatcherExpr = LinearMatcher.genMatcher(prog)
        // val linearMatcherExpr = VMCodegenFlat.genMatcherFlat(prog)
        // val machineMatcherExpr = StagedMachine.genMachineMatcher(prog)
        // val buildMachineTable = StagedMachine.buildMachineTable(prog)
        // val stepLoopExpr = StagedMachine.generateStepLoop(prog)
        // quotes.reflect.report.info(matcherExpr.show)
        // val funcTable = LinearMatcher.buildTable(prog)
        // println(backtrackProgMatcherExpr.show)
        '{
            new oregano.Regex[List[String]] {
                val regex = ${Expr(s)}.r
                val re2 = ${Expr(s)}.r2
                val prog = $liftedProgExpr
                val runtimeMachine = RE2Machine(prog)
                val compileTimeMachine = RE2Machine(prog)
                // val machineTable = $buildMachineTable
                // re2machine.init(prog.numCap)
                val q0 = ThreadQueue(prog.numInst)
                val q1 = ThreadQueue(prog.numInst)
                // val re2Regex = ${Expr(s)}.r2
                // val prog = $liftedProgExpr
                // val flatTable = $flatTableExpr
                // def matches(input: CharSequence): Boolean = regex.matches(input)
                // def matches(input: CharSequence): Boolean = $backtrackProgMatcherExpr(input)
                // def matchesRuntimeLinear(input: CharSequence): Boolean = MatcherFlat.matches(input, flatTable)
                // // def matchesRuntimeLinear(input: CharSequence): Boolean = MatcherFlatProg.matches(prog, input)
                // // def matchesRuntimeLinear(input: CharSequence): Boolean = {
                // //   re2Regex.matches(input)
                // // }   
                // def matchesRuntimeBacktrack(input: CharSequence): Boolean = VMCodegenBacktracking.matches(prog, input)
                def matches(input: CharSequence): Boolean = re2.matches(input)
                // def matchesLinear(input: CharSequence): Boolean = $linearMatcherExpr(input, flatTable, q0, q1)
                def matchesLinear(input: CharSequence): Boolean = compileTimeMachine.matches(input)
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