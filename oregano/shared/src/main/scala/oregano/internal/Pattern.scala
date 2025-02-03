package oregano.internal

import scala.quoted.*

sealed trait Pattern {
  def optimize: Pattern = this match {
    case Pattern.Cat(ps) =>
      Pattern.Cat(ps.flatMap {
        case Pattern.Cat(subs) => subs.map(_.optimize)
        case p => List(p.optimize)
      })
    case Pattern.Alt(p1, p2) =>
      Pattern.Alt(p1.optimize, p2.optimize)
    case other => other
  }

  def matchRuntime(input: String): Boolean = ???
  def matchHere(input: CharSequence, pos: Int): Option[Int]
  def matches(input: CharSequence): Boolean = matchHere(input, 0).contains(input.length)
}

object Pattern {
  final case class Lit(c: Int) extends Pattern {
    def matchHere(input: CharSequence, pos: Int): Option[Int] =
      if (pos < input.length && input.charAt(pos) == c.toChar) Some(pos + 1) else None
  }
  final case class Cat(patterns: List[Pattern]) extends Pattern {
    def matchHere(input: CharSequence, pos: Int): Option[Int] =
      patterns.foldLeft(Option(pos)) { (optPos, pat) =>
        optPos.flatMap(p => pat.matchHere(input, p))
      }
  }
  final case class Alt(left: Pattern, right: Pattern) extends Pattern {
     def matchHere(input: CharSequence, pos: Int): Option[Int] =
      left.matchHere(input, pos) orElse right.matchHere(input, pos)
  }

  def lit(c: Int): Pattern = Lit(c)
  def concat(ps: Pattern*): Pattern = Cat(ps.toList).optimize
  def alt(p1: Pattern, p2: Pattern): Pattern = Alt(p1, p2).optimize

  def compile(regex: Regex): Pattern = regex match {
    case Regex.Lit(c) => Pattern.Lit(c)
    case Regex.Cat(rs) => Pattern.Cat(rs.map(compile))
    case Regex.Alt(r1, r2) => Pattern.Alt(compile(r1), compile(r2))
    case _ => ???
  }
}

private def inlineMatchExpr(
    p: Pattern,
    inputExpr: Expr[CharSequence],
    posExpr: Expr[Int]
)(using Quotes): Expr[Int] =
  p match {
    case Pattern.Lit(c) =>
      '{
        if ($posExpr < $inputExpr.length && $inputExpr.charAt($posExpr) == ${Expr(c.toChar)}) $posExpr + 1
        else -1
      }
    case Pattern.Cat(patterns) =>
      patterns.foldLeft(posExpr) { (accPosExpr, sub) =>
        '{
          if ($accPosExpr < 0) -1 // short-circuit
          else {
            val nextPos = ${ inlineMatchExpr(sub, inputExpr, accPosExpr) }
            nextPos
          }
        }
      }
    case Pattern.Alt(left, right) =>
      '{
        val leftPos = ${ inlineMatchExpr(left, inputExpr, posExpr) }
        if (leftPos >= 0) leftPos
        else ${ inlineMatchExpr(right, inputExpr, posExpr) }
      }
  }


@main def testPatterns() =
  val p1 = Pattern.concat(
    Pattern.lit('a'),
    Pattern.concat(Pattern.lit('b'), Pattern.lit('c'))
  )
  println(p1)

  val p2 = Pattern.alt(
    Pattern.lit('x'),
    Pattern.concat(Pattern.lit('y'), Pattern.lit('z'))
  )
  println(p2)

  println(p2 `matches` "x")
