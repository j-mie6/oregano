/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import scala.quoted.*
import codes.quine.labo.re2s._

// TODO: move to utils or something
def countNodes(expr: Expr[Any])(using Quotes): Int =
  import quotes.reflect.*
  // println(expr.show)

  def loop(tree: Tree): Int = 
    tree match
      case Inlined(_, _, body) => 1 + loop(body)
      case Block(stats, expr) => 1 + stats.map(loop).sum + loop(expr)
      case Apply(fun, args) => 1 + loop(fun) + args.map(loop).sum
      case TypeApply(fun, args) => 1 + loop(fun) + args.map(loop).sum
      case Select(qualifier, _) => 1 + loop(qualifier)
      case Ident(_) => 1
      case Literal(_) => 1
      case If(cond, thenp, elsep) => 1 + loop(cond) + loop(thenp) + loop(elsep)
      case Match(selector, cases) => 1 + loop(selector) + cases.map {
                                            case CaseDef(_, guard, rhs) =>
                                              1 + guard.map(loop).getOrElse(0) + loop(rhs)
                                          }.sum
      case ValDef(_, _, rhsOpt) => 1 + rhsOpt.map(loop).getOrElse(0)
      case DefDef(_, _, _, bodyOpt) => 1 + bodyOpt.map(loop).getOrElse(0)
      case Typed(expr, _) => 1 + loop(expr)
      case This(_) => 1
      case New(_) => 1
      case While(cond, body) => 1 + loop(cond) + loop(body)
      case Return(expr, _) => 1 + loop(expr)
      case Assign(lhs, rhs) => 1 + loop(lhs) + loop(rhs)
      case Repeated(elems, _) => 1 + elems.map(loop).sum
      case Lambda(_, body) => 1 + loop(body)
      case Closure(meth, tpeOpt) => 1 + loop(meth) + tpeOpt.map(_ => 1).getOrElse(0)
      case _: TypeTree => 1
      case _ => 
        // This is essentially to flag if I missed something: this function has been grokked from staring at Scala compiler source code!
        println(s"Unhandled tree type: ${tree.getClass.getSimpleName}")
        1  // final catch-all for safety

  loop(expr.asTerm)

private inline def selectLinearBehaviour(inline bool: Boolean)(using Quotes): Boolean = ???

private [oregano] def compileMacro(s: String)(using Quotes): Expr[oregano.Regex[?]] = {
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
        val backtrackingMatcherExpr: Expr[CharSequence => Option[Array[Int]]] = 
          if patternResult.flatControlFlow then
            BacktrackingProgMatcher.genMatcherWithCaps(prog)
          else
            CPSMatcher.genMatcherPatternWithCaps(p, groupCount)
        // useful for debugging: 
        // val backtrackCPSMatcherExpr = CPSMatcher.genMatcherPatternWithCaps(p, groupCount)
        // val backtrackProgMatcherExpr = BacktrackingProgMatcher.genMatcherWithCaps(prog)


        // Linear matching stuff
        val linearMatcherExpr = StagedMachine.generateStepLoop(prog)
        val willBeLinearStaged = countNodes(linearMatcherExpr) < 4500
        if (!willBeLinearStaged) then report.warning(s"Matcher for $s is too large, will not be linear staged")

        // for testing:
        // val nodeCount = countNodes(linearMatcherExpr)
        // println(s"regex: $s counted nodes: $nodeCount, numInst: ${prog.numInst}")
        '{
            new oregano.Regex[List[String]] {
                val regex = ${Expr(s)}.r
                val re2 = ${Expr(s)}.r2
                val prog = $liftedProgExpr
                val re2Machine = RE2Machine(prog)

                def matches(input: CharSequence): Boolean = $backtrackingMatcherExpr(input).isDefined
                def matchesWithCaps(input: CharSequence): Option[Array[Int]] = $backtrackingMatcherExpr(input)
                def matchesLinear(input: CharSequence): Boolean = 
                    // This should be evaluated at compile time: does not explode for large regexes
                    if ${Expr(willBeLinearStaged)} then 
                        $linearMatcherExpr(re2Machine, input) 
                    else 
                        re2Machine.matches(input)

                def unapplySeq(input: CharSequence): Option[List[String]] = regex.unapplySeq(input)
            }
        }
    case Left(err) => report.errorAndAbort(err)
}
