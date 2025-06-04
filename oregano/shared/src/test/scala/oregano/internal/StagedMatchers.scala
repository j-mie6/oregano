package oregano.internal

import scala.quoted.*

object StagedMatchers:

  /** Compile-time staged matcher using CPS engine */
  inline def stagedCPS(regex: String): CharSequence => Boolean =
    ${ stagedCPSImpl('regex) }

  inline def stagedCPSWithCaps(regex: String): CharSequence => Option[Array[Int]] =
    ${ stagedCPSWithCapsImpl('regex) }

  private def stagedCPSImpl(regexExpr: Expr[String])(using Quotes): Expr[CharSequence => Boolean] =
    val regex = regexExpr.valueOrAbort
    val PatternResult(pattern, _, _, _) = Pattern.compile(regex)
    CPSMatcher.genMatcherPattern(pattern)

  private def stagedCPSWithCapsImpl(regexExpr: Expr[String])(using Quotes): Expr[CharSequence => Option[Array[Int]]] =
    val regex = regexExpr.valueOrAbort
    val PatternResult(pattern, groupCount, _, _) = Pattern.compile(regex)
    CPSMatcher.genMatcherPatternWithCaps(pattern, groupCount)


  /** Compile-time staged matcher using Backtracking engine */
  inline def stagedProg(regex: String): CharSequence => Boolean =
    ${ stagedProgImpl('regex) }

  inline def stagedProgWithCaps(regex: String): CharSequence => Option[Array[Int]] =
    ${ stagedProgWithCapsImpl('regex) }

  private def stagedProgImpl(regexExpr: Expr[String])(using Quotes): Expr[CharSequence => Boolean] =
    val regex = regexExpr.valueOrAbort
    val PatternResult(pattern, groupCount, _, _) = Pattern.compile(regex)
    val prog = ProgramCompiler.compileRegexp(pattern, groupCount)
    BacktrackingProgMatcher.genMatcher(prog)

  private def stagedProgWithCapsImpl(regexExpr: Expr[String])(using Quotes): Expr[CharSequence => Option[Array[Int]]] =
    val regex = regexExpr.valueOrAbort
    val PatternResult(pattern, groupCount, _, _) = Pattern.compile(regex)
    val prog = ProgramCompiler.compileRegexp(pattern, groupCount)
    BacktrackingProgMatcher.genMatcherWithCaps(prog)
