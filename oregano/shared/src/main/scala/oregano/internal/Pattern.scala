package oregano.internal

import scala.quoted.*
import cats.collections.{Diet, Range}

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
}

final case class PatternResult(
  pattern: Pattern,
  groupCount: Int,
  flatControlFlow: Boolean,
  numReps: Int
)

class PatternBuilder {
  var nextGroup: Int = 1 // note that 1 is reserved for the whole match, as with other engines
  var numReps = 0 // initially used for caching nested Rep0 loops safely: TODO: doesn't work and isn't neccessary, delete!

  def compile(regex: Regex): Pattern = regex match {
    case Regex.Lit(c) => 
      Pattern.Lit(c)

    case Regex.Cat(rs) =>
      rs.foldLeft(List.empty[Pattern]) {
        case (acc, r) =>
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
      Pattern.Class(Regex.AllSet -- Diet.one('\n'.toInt)) // Dot matches any character except newline, there is a flag to change this

    case _ =>
      throw IllegalArgumentException(s"Unsupported regex: $regex")
  }

  def build(regex: Regex): PatternResult = 
    val pattern = compile(regex)
    val groupCount = nextGroup
    val stageable = Pattern.checkFlatControlFlow(pattern)
    val numReps = this.numReps + 1
    PatternResult(pattern, groupCount, stageable, numReps)
}

object Pattern {
  final case class Lit(c: Int) extends Pattern 
  final case class Cat(patterns: List[Pattern]) extends Pattern 
  final case class Alt(left: Pattern, right: Pattern) extends Pattern 
  final case class Class(diet: Diet[Int]) extends Pattern
  final case class Rep0(pat: Pattern, idx: Int) extends Pattern
  final case class Capture(groupIdx: Int, pat: Pattern) extends Pattern

  def lit(c: Int): Pattern = Lit(c)
  def concat(ps: Pattern*): Pattern = Cat(ps.toList)
  def alt(p1: Pattern, p2: Pattern): Pattern = Alt(p1, p2)
  def charClass(diet: Diet[Int]): Pattern = Class(diet)
  def rep0(pat: Pattern): Pattern = Rep0(pat, 0) // idx is not used here

  // Do we need this? could we not use Regex.Lit, Regex.Cat, Regex.Alt etc directly?
  // Might be worth having when optimising, for methods etc. but not sure
  def compile(regex: Regex, nextGroup: Int = 1): PatternResult = 
    // regex match 
    //   case Regex.Lit(c) => 
    //     (Pattern.Lit(c), nextGroup)

    //   case Regex.Cat(rs) =>
    //     rs.foldLeft((List.empty[Pattern], nextGroup)) {
    //       case ((acc, g), r) =>
    //         val (p, g2) = compile(r, g)
    //         (acc :+ p, g2)
    //     } match {
    //       case (ps, g) => (Pattern.Cat(ps), g)
    //     }

    //   case Regex.Alt(r1, r2) =>
    //     val (p1, g1) = compile(r1, nextGroup)
    //     val (p2, g2) = compile(r2, g1)
    //     (Pattern.Alt(p1, p2), g2)

    //   case Regex.Class(d) =>
    //     (Pattern.Class(d), nextGroup)

    //   case Regex.Rep0(r) =>
    //     val (p, g) = compile(r, nextGroup)
    //     (Pattern.Rep0(p), g)

    //   case Regex.Capture(r) =>
    //     val groupId = nextGroup
    //     val (p, g2) = compile(r, nextGroup + 1)
    //     (Pattern.Capture(groupId, p), g2)

    //   case Regex.NonCapture(r) => compile(r, nextGroup)

    //   case _ =>
    //     throw IllegalArgumentException(s"Unsupported regex: $regex")
    val pat = new PatternBuilder()
    pat.build(regex)

  def checkForNestedLoop(pat: Pattern, seenLoop: Boolean = false): Boolean = 
    pat match {
      case Pattern.Rep0(_, _) if seenLoop => true
      case Pattern.Rep0(p, _) => checkForNestedLoop(p, true)
      case Pattern.Alt(left, right) => checkForNestedLoop(left, seenLoop) || checkForNestedLoop(right, seenLoop)
      case Pattern.Cat(ps) => ps.exists(p => checkForNestedLoop(p, seenLoop))
      case Pattern.Capture(_, p) => checkForNestedLoop(p, seenLoop)
      case _ => false
    }

  def checkFlatControlFlow(pat: Pattern): Boolean =
    // for now, protect against nested loops
    !checkForNestedLoop(pat)

  def compile(regex: String): PatternResult =
    val re: Regex = parse(regex).getOrElse(throw IllegalArgumentException(s"Invalid regex: $regex"))
    compile(re)
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