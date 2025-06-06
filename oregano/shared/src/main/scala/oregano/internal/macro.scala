/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import scala.quoted.*
// import codes.quine.labo.re2s._

private[oregano] def compileMacro(
    s: String
)(using Quotes): Expr[oregano.Regex[?]] = {
  import quotes.reflect.report
  parse(s) match
    case Right(ast) =>
      // report.info(s"$ast")
      val patternResult = Pattern.compile(ast)
      // report.info(s"Parsley AST: ${ast.toString}\nPattern: ${patternResult.pattern}, groupCount: ${patternResult.groupCount}")
      val p = patternResult.pattern
      val groupCount = patternResult.groupCount
      val prog = ProgramCompiler.compileRegexp(p, groupCount)
      // report.info(s"Prog:\n$prog")
      val liftedProgExpr = Expr(prog)
      // println(s"Prog:\n$prog")

      // backtracking matcher stuff
      val backtrackingMatcherWithCapsExpr
          : Expr[CharSequence => Option[Array[Int]]] =
        if patternResult.flatControlFlow then
          BacktrackingProgMatcher.genMatcherWithCaps(prog)
        else CPSMatcher.genMatcherPatternWithCaps(p, groupCount)

      val backtrackingMatcherExpr: Expr[CharSequence => Boolean] =
        if patternResult.flatControlFlow then
          BacktrackingProgMatcher.genMatcher(prog)
        else CPSMatcher.genMatcherPattern(p)

      val backtrackingFinderExpr: Expr[CharSequence => Boolean] =
        if patternResult.flatControlFlow then
          BacktrackingProgMatcher.genFind(prog)
        else CPSMatcher.genFinderPattern(p, groupCount)
      // useful for debugging:
      // val backtrackCPSMatcherExpr = CPSMatcher.genMatcherPattern(p)
      // val backtrackingCPSMatcherWithCaps = CPSMatcher.genMatcherPatternWithCaps(p, groupCount)
      // val backtrackProgMatcherExpr = BacktrackingProgMatcher.genMatcher(prog)
      // val backtrackProgMatcherWithCaps = BacktrackingProgMatcher.genMatcherWithCaps(prog)

      // Linear matching stuff
      var linearMatcherExpr = StagedMachine.generateStepLoop(prog)
      val willBeLinearStaged =
        Utils.countNodes(linearMatcherExpr) < 4500 // heuristic
      if (!willBeLinearStaged) then
        report.warning(
          s"Matcher for $s is too large, will not be linear staged"
        )
        // set linearMatcherExpr to just use runtime
        linearMatcherExpr = '{ (m: RE2Machine, input: CharSequence) =>
          m.matches(input)
        }

      // for testing:
      // val nodeCount = Utils.countNodes(backtrackProgMatcherExpr)
      // println(s"regex: $s counted nodes: $nodeCount, numInst: ${prog.numInst}")
      // val nodeCountWithCaps = Utils.countNodes(backtrackProgMatcherWithCaps)
      // println(s"regex: $s counted nodes: $nodeCountWithCaps, numInst: ${prog.numInst}")
      '{
        new oregano.Regex[List[String]] {
          val regex = ${ Expr(s) }.r
          // val re2 = ${Expr(s)}.r2
          val prog = $liftedProgExpr
          val re2Machine = RE2Machine(prog)

          def matches(input: CharSequence): Boolean =
            $backtrackingMatcherExpr(input)
          // def matches(input: CharSequence): Boolean = regex.matches(input)
          def matchesWithCaps(input: CharSequence): Option[Array[Int]] =
            $backtrackingMatcherWithCapsExpr(input)
          def matchesLinear(input: CharSequence): Boolean =
            $linearMatcherExpr(re2Machine, input)
          def find(input: CharSequence): Boolean =
            $backtrackingFinderExpr(input)
          // def matchesLinear(input: CharSequence): Boolean = re2Machine.matches(input)
          def unapplySeq(input: CharSequence): Option[List[String]] =
            matchesWithCaps(input).map { caps =>
              caps
                .drop(2)
                .sliding(2, 2)
                .collect {
                  case Array(start, end) if start != -1 && end != -1 =>
                    input.subSequence(start, end).toString
                }
                .toList
            }
        }
      }
    case Left(err) => report.errorAndAbort(err)
}
