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

object Pattern {
  final case class Lit(c: Int) extends Pattern 
  final case class Cat(patterns: List[Pattern]) extends Pattern 
  final case class Alt(left: Pattern, right: Pattern) extends Pattern 
  final case class Class(diet: Diet[Int]) extends Pattern
  final case class Rep0(pat: Pattern) extends Pattern
  final case class Capture(groupIdx: Int, pat: Pattern) extends Pattern

  def lit(c: Int): Pattern = Lit(c)
  def concat(ps: Pattern*): Pattern = Cat(ps.toList).optimize
  def alt(p1: Pattern, p2: Pattern): Pattern = Alt(p1, p2).optimize
  def charClass(diet: Diet[Int]): Pattern = Class(diet)
  def rep0(pat: Pattern): Pattern = Rep0(pat)

  // Do we need this? could we not use Regex.Lit, Regex.Cat, Regex.Alt etc directly?
  // Might be worth having when optimising, for methods etc. but not sure
  def compile(regex: Regex, nextGroup: Int = 1): (Pattern, Int) = regex match {
      case Regex.Lit(c) => 
        (Pattern.Lit(c), nextGroup)

      case Regex.Cat(rs) =>
        rs.foldLeft((List.empty[Pattern], nextGroup)) {
          case ((acc, g), r) =>
            val (p, g2) = compile(r, g)
            (acc :+ p, g2)
        } match {
          case (ps, g) => (Pattern.Cat(ps), g)
        }

      case Regex.Alt(r1, r2) =>
        val (p1, g1) = compile(r1, nextGroup)
        val (p2, g2) = compile(r2, g1)
        (Pattern.Alt(p1, p2), g2)

      case Regex.Class(d) =>
        (Pattern.Class(d), nextGroup)

      case Regex.Rep0(r) =>
        val (p, g) = compile(r, nextGroup)
        (Pattern.Rep0(p), g)

      case Regex.Capture(r) =>
        val groupId = nextGroup
        val (p, g2) = compile(r, nextGroup + 1)
        (Pattern.Capture(groupId, p), g2)

      case Regex.NonCapture(r) => compile(r, nextGroup)

      case _ =>
        throw IllegalArgumentException(s"Unsupported regex: $regex")
    }

  def compile(regex: String): (Pattern, Int) =
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

