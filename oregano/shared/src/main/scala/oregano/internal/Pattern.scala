package oregano.internal

import scala.quoted.*
import cats.collections.{Diet, Range}

given ToExpr[Range[Int]] with
  def apply(rng: Range[Int])(using Quotes): Expr[Range[Int]] =
    '{ Range(${ Expr(rng.start) }, ${ Expr(rng.end) }) }

given ToExpr[Diet[Int]] with
  def apply(diet: Diet[Int])(using Quotes): Expr[Diet[Int]] =
    diet.toIterator.foldLeft('{ Diet.empty[Int] }) { (acc, rng) =>
      val rngExpr = Expr(rng)
      '{ $acc.addRange($rngExpr) }
    }

enum Pattern {
  case Lit(c: Int)
  case Cat(patterns: List[Pattern])
  case Alt(left: Pattern, right: Pattern)
  case Class(diet: Diet[Int])
  case Rep0(pat: Pattern, idx: Int)
  case Capture(groupIdx: Int, pat: Pattern)

  def optimize: Pattern = this match {
    case Cat(ps) =>
      Cat(ps.flatMap {
        case Cat(subs) => subs.map(_.optimize)
        case p         => List(p.optimize)
      })
    case Alt(p1, p2) =>
      Alt(p1.optimize, p2.optimize)
    case _ => this
  }
}

final case class PatternResult(
    pattern: Pattern,
    groupCount: Int,
    flatControlFlow: Boolean,
    numReps: Int
)

class PatternBuilder {
  var nextGroup: Int =
    1 // note that 1 is reserved for the whole match, as with other engines
  var numReps =
    0 // initially used for caching nested Rep0 loops safely: TODO: doesn't work and isn't neccessary, delete!

  def compile(regex: Regex): Pattern = regex match {
    case Regex.Lit(c) =>
      Pattern.Lit(c)

    case Regex.Cat(rs) =>
      rs.foldLeft(List.empty[Pattern]) { case (acc, r) =>
        val p = compile(r)
        acc :+ p
      } match {
        case ps => Pattern.Cat(ps)
      }

    case Regex.Alt(r1, r2) =>
      val p1 = compile(r1)
      val p2 = compile(r2)
      Pattern.Alt(p1, p2)

    case Regex.Class(d) =>
      Pattern.Class(d)

    case Regex.Rep0(r) =>
      val p = compile(r)
      val idx = numReps
      numReps += 1
      Pattern.Rep0(p, idx)

    // Given we use a shared `p`, capture indicies are propogated safely so I believe this to be safe
    // That being said, not doing this could yield a more terse Prog, but I don't have time
    // I'd expect the more terse Prog to be more performant
    case Regex.Rep1(r) =>
      val p = compile(r)
      val idx = numReps
      numReps += 1
      Pattern.Cat(List(p, Pattern.Rep0(p, idx)))

    case Regex.Capture(r) =>
      val groupId = nextGroup
      nextGroup += 1
      val p = compile(r)
      Pattern.Capture(groupId, p)

    case Regex.NonCapture(r) => compile(r)

    case Regex.Dot =>
      // Dot matches any character except newline, there is a flag to change this, could be handled
      // Could keep a Pattern.Dot, but for now, we can use a class that matches all characters except newline as is default
      Pattern.Class(Regex.AllSet -- Diet.one('\n'.toInt))

    case _ =>
      throw IllegalArgumentException(s"Unsupported regex: $regex")
  }

  def build(regex: Regex): PatternResult =
    val pattern = compile(regex)
    val groupCount = nextGroup
    val flatControlFlow = Pattern.checkFlatControlFlow(pattern)
    val numReps = this.numReps + 1
    PatternResult(pattern, groupCount, flatControlFlow, numReps)
}

object Pattern {
  def lit(c: Int): Pattern = Pattern.Lit(c)
  def concat(ps: Pattern*): Pattern = Pattern.Cat(ps.toList)
  def alt(p1: Pattern, p2: Pattern): Pattern = Pattern.Alt(p1, p2)
  def charClass(diet: Diet[Int]): Pattern = Pattern.Class(diet)
  def rep0(pat: Pattern): Pattern = Pattern.Rep0(pat, 0) // idx is not used here

  def compile(regex: Regex): PatternResult =
    val pat = new PatternBuilder()
    pat.build(regex)

  def checkForNestedLoop(pat: Pattern, seenLoop: Boolean = false): Boolean =
    pat match {
      case Pattern.Rep0(_, _) if seenLoop => true
      case Pattern.Rep0(p, _)             => checkForNestedLoop(p, true)
      case Pattern.Alt(left, right)       =>
        checkForNestedLoop(left, seenLoop) || checkForNestedLoop(
          right,
          seenLoop
        )
      case Pattern.Cat(ps) => ps.exists(p => checkForNestedLoop(p, seenLoop))
      case Pattern.Capture(_, p) => checkForNestedLoop(p, seenLoop)
      case _                     => false
    }

  def checkFlatControlFlow(pat: Pattern): Boolean =
    // for now, protect against nested loops
    !checkForNestedLoop(pat)

  def compile(regex: String): PatternResult =
    val re: Regex = parse(regex).getOrElse(
      throw IllegalArgumentException(s"Invalid regex: $regex")
    )
    compile(re)
}

given ToExpr[Pattern] with
  def apply(pat: Pattern)(using Quotes): Expr[Pattern] = pat match {
    case Pattern.Lit(c)           => '{ Pattern.Lit(${ Expr(c) }) }
    case Pattern.Cat(patterns)    => '{ Pattern.Cat(${ Expr(patterns) }) }
    case Pattern.Alt(left, right) =>
      '{ Pattern.Alt(${ Expr(left) }, ${ Expr(right) }) }
    case Pattern.Class(diet)    => '{ Pattern.Class(${ Expr(diet) }) }
    case Pattern.Rep0(pat, idx) =>
      '{ Pattern.Rep0(${ Expr(pat) }, ${ Expr(idx) }) }
    case Pattern.Capture(groupIdx, pat) =>
      '{ Pattern.Capture(${ Expr(groupIdx) }, ${ Expr(pat) }) }
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

  // check if nested loop detection works
  val PatternResult(unnestedLoop, _, _, _) = Pattern.compile("ab*c*")
  println(Pattern.checkFlatControlFlow(unnestedLoop)) // should be false

  val PatternResult(nestedLoop, _, _, _) = Pattern.compile("a(b*)*c*")
  println(nestedLoop)
  println(Pattern.checkFlatControlFlow(nestedLoop))
