package oregano.internal

import scala.quoted.*
// import oregano.internal.Pattern.matchPattern
import cats.collections.{Diet, Range}

// Need to convert at runtime
// TODO: See how existing Matcher class deals with this, doesn't seem egregious
// given ToExpr[CharSequence] with
//   def apply(input: CharSequence)(using Quotes): Expr[CharSequence] =
//     Expr(input.toString)

given ToExpr[Range[Int]] with
  def apply(rng: Range[Int])(using Quotes): Expr[Range[Int]] =
    '{ Range(${Expr(rng.start)}, ${Expr(rng.end)}) }

given ToExpr[Diet[Int]] with
  def apply(diet: Diet[Int])(using Quotes): Expr[Diet[Int]] =
    diet.toIterator.foldLeft('{ Diet.empty[Int] }) { (acc, rng) =>
      val rngExpr = Expr(rng)
      '{ $acc.addRange($rngExpr) }
    }

sealed trait Pattern {
  def optimize: Pattern = this match {
    case Pattern.Cat(ps) =>
      Pattern.Cat(ps.flatMap {
        case Pattern.Cat(subs) => subs.map(_.optimize)
        case p => List(p.optimize)
      })
    case Pattern.Alt(p1, p2) =>
      Pattern.Alt(p1.optimize, p2.optimize)
    case _ => this
  }

//   def matches(input: CharSequence): Boolean = matchPattern(this, input, 0) == input.length
//   def matches(input: Expr[CharSequence])(using Quotes): Expr[Boolean] = '{ ${ matchPattern(this, input, Expr(0)) } == $input.length }
}

object Pattern {
  final case class Lit(c: Int) extends Pattern 
  final case class Cat(patterns: List[Pattern]) extends Pattern 
  final case class Alt(left: Pattern, right: Pattern) extends Pattern 
  final case class Class(diet: Diet[Int]) extends Pattern
  final case class Rep0(pat: Pattern) extends Pattern

  def lit(c: Int): Pattern = Lit(c)
  def concat(ps: Pattern*): Pattern = Cat(ps.toList).optimize
  def alt(p1: Pattern, p2: Pattern): Pattern = Alt(p1, p2).optimize
  def charClass(diet: Diet[Int]): Pattern = Class(diet)
  def rep0(pat: Pattern): Pattern = Rep0(pat)

  // Do we need this? could we not use Regex.Lit, Regex.Cat, Regex.Alt etc directly?
  // Might be worth having when optimising, for methods etc. but not sure
  def compile(regex: Regex): Pattern = regex match {
    case Regex.Lit(c) => Pattern.Lit(c)
    case Regex.Cat(rs) => Pattern.Cat(rs.map(compile))
    case Regex.Alt(r1, r2) => Pattern.Alt(compile(r1), compile(r2))
    case Regex.Class(diet) => Pattern.Class(diet)
    case Regex.Rep0(r) => Pattern.Rep0(compile(r))
    case _ => ???
  }

//   def matchPattern(p: Pattern, input: CharSequence, pos: Int): Int = p match {
//     case Lit(c) =>
//       if (pos < input.length() && input.charAt(pos) == c.toChar) pos + 1 else -1

//     case Cat(patterns) =>
//       patterns.foldLeft(pos) { (accPos, pat) =>
//         if (accPos < 0) -1 else matchPattern(pat, input, accPos)
//       }

//     case Alt(left, right) =>
//       val leftPos = matchPattern(left, input, pos)
//       if (leftPos >= 0) leftPos else matchPattern(right, input, pos)
  
//     case Class(diet) =>
//       if (pos < input.length() && diet.contains(input.charAt(pos).toInt)) pos + 1
//       else -1
//   }

//   def matchPattern(
//       p: Pattern,
//       inputExpr: Expr[CharSequence],
//       posExpr: Expr[Int]
//   )(using Quotes): Expr[Int] =
//     // println("inlining matchPattern") // debug
//     p match {
//       case Pattern.Lit(c) =>
//         '{
//           if ($posExpr < $inputExpr.length && $inputExpr.charAt($posExpr) == ${Expr(c.toChar)}) $posExpr + 1
//           else -1
//         }

//       case Pattern.Cat(patterns) =>
//         patterns.foldLeft(posExpr) { (accPosExpr, sub) =>
//           '{
//             if ($accPosExpr < 0) -1 // short-circuit
//             else {
//               val nextPos = ${ matchPattern(sub, inputExpr, accPosExpr) }
//               nextPos
//             }
//           }
//         }

//       case Pattern.Alt(left, right) =>
//         '{
//           val leftPos = ${ matchPattern(left, inputExpr, posExpr) }
//           if (leftPos >= 0) leftPos
//           else ${ matchPattern(right, inputExpr, posExpr) }
//         }

//       case Class(diet) => 
//         // no idea why Expr is needed, '{ diet } doesn't work
//         '{
//           if ($posExpr < $inputExpr.length && ${ Expr(diet) }.contains($inputExpr.charAt($posExpr).toInt)) $posExpr + 1
//           else -1
//         }
//     }

  // not sure if needed, more for testing
  def compile(regex: String): Pattern = compile(parse(regex).getOrElse(throw IllegalArgumentException(s"Invalid regex: $regex")))
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

