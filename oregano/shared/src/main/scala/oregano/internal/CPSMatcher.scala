package oregano.internal

import scala.quoted.*
import cats.collections.Diet

final case class MatchResult(input: CharSequence, matches: Array[Int]) {
  def start(group: Int): Int = matches(2 * group)
  def end(group: Int): Int = matches(2 * group + 1)
  def group(group: Int): String =
    input.subSequence(start(group), end(group)).toString
}

// // essentially copy matchRuneExpr; todo: use a common function?
def dietContains(diet: Diet[Int])(using Quotes): Expr[Int] => Expr[Boolean] = {
  val runes: List[Int] = Utils.dietToRanges(diet)

  val pairs: List[(Int, Int)] = runes
    .grouped(2)
    .collect { case List(lo, hi) => (lo, hi) }
    .toList

  (r: Expr[Int]) => {
    val conditions: List[Expr[Boolean]] = pairs.map { case (lo, hi) =>
      if lo == hi then '{ $r == ${Expr(lo)} }
      else '{ $r >= ${Expr(lo)} && $r <= ${Expr(hi)} }
    }
    conditions.reduceLeft((a, b) => '{ $a || $b })
  }
}


object CPSMatcher:
  private def compile(
      p: Pattern,
      input: Expr[CharSequence],
      noCaps: Int,
      pos: Expr[Int],
      cont: Expr[Int] => Expr[Int],
      withCaps: Boolean,
      groupsExpr: Expr[Array[Int]]
  )(using Quotes): Expr[Int] =

    p match
      case Pattern.Lit(c) =>
        '{
          if $pos < $input.length && $input.charAt($pos) == ${Expr(c)} then
            ${ cont('{ $pos + 1 }) }
          else -1
        }

      case Pattern.Class(diet) =>
        val runeCheck: Expr[Int] => Expr[Boolean] = dietContains(diet)
        val condExpr: Expr[Boolean] = runeCheck('{ $input.charAt($pos).toInt })
        '{
          if $pos < $input.length && $condExpr
          then ${ cont('{ $pos + 1 }) }
          else -1
        }

      case Pattern.Cat(ps) =>
        ps.foldRight(cont) { (sub, next) =>
          (p: Expr[Int]) => compile(sub, input, noCaps, p, next, withCaps, groupsExpr)
        }(pos)

      case Pattern.Alt(p1, p2) =>
        val left  = compile(p1, input, noCaps, pos, cont, withCaps, groupsExpr)
        val right = compile(p2, input, noCaps, pos, cont, withCaps, groupsExpr)
        '{ val lp = $left; if lp >= 0 then lp else $right }

      case Pattern.Rep0(sub, _) =>
        '{
          def self(p: Int): Int =
            val step = ${
              compile(
                sub,
                input,
                noCaps,
                '{ p },
                (next: Expr[Int]) => '{ if $next != p then self($next) else -1 },
                withCaps,
                groupsExpr
              )
            }
            if step >= 0 then step else ${ cont('{ p }) }
          self($pos)
        }

      case Pattern.Capture(idx, sub) =>
        if !withCaps || idx >= noCaps then
          compile(sub, input, noCaps, pos, cont, false, '{ null })
        else
          val startIdx = Expr(2 * idx)
          val endIdx   = Expr(2 * idx + 1)

          val newCont: Expr[Int] => Expr[Int] = (endPos: Expr[Int]) =>
            '{
              val savedEnd = $groupsExpr($endIdx)
              $groupsExpr($endIdx) = $endPos
              val res = ${ cont(endPos) }
              if res >= 0 then res
              else {
                $groupsExpr($endIdx) = savedEnd
                -1
              }
            }

          val inner = compile(sub, input, noCaps, pos, newCont, true, groupsExpr)

          '{
            val savedStart = $groupsExpr($startIdx)
            $groupsExpr($startIdx) = $pos
            val res = $inner
            if res >= 0 then res
            else {
              $groupsExpr($startIdx) = savedStart
              -1
            }
          }

  def genMatcherPattern(pattern: Pattern)(using Quotes): Expr[CharSequence => Boolean] =
    '{
      (input: CharSequence) =>
        val result = ${
          val cont = (i: Expr[Int]) => '{ if $i == input.length then $i else -1 }
          compile(pattern, 'input, 0, '{ 0 }, cont, false, '{ null })
        }
        result == input.length
    }

  def genMatcherPatternWithCaps(pattern: Pattern, numGroups: Int)(using Quotes): Expr[CharSequence => Option[Array[Int]]] =
    '{
      (input: CharSequence) =>
        val inputLen = input.length
        val groups = Array.fill(${Expr(numGroups * 2)})(-1)
        groups(0) = 0

        val result = ${
          val cont = (i: Expr[Int]) => '{ if $i == inputLen then $i else -1 }
          compile(pattern, 'input, numGroups, '{ 0 }, cont, withCaps = true, 'groups)
        }

        if result == inputLen then
          groups(1) = result
          Some(groups)
        else None
    }

  def genPrefixFinderPattern(pattern: Pattern, numGroups: Int)(using Quotes): Expr[(Int, CharSequence) => Int] =
    '{
      (startPos: Int, input: CharSequence) =>
        ${
          val cont = (i: Expr[Int]) => i
          compile(pattern, 'input, numGroups, 'startPos, cont, withCaps = false, '{ null })
        }
    }

  def makeMatcher(
      pattern: Pattern,
      numGroups: Int,
  ): (CharSequence, Boolean) => Option[Array[Int]] = {
    def matcher(input: CharSequence, anchorEnd: Boolean): Option[Array[Int]] = {
      val inputLen = input.length
      val groups: Array[Int] = Array.fill(numGroups * 2)(-1)
      groups(0) = 0

      val endCont: (Int, Array[Int]) => Int =
        if anchorEnd then (i, _) => if i == inputLen then i else -1
        else
          (i, _) =>
            if i != -1 then i
            else -1 // no specific end condition, just return the position

      def compile(
          p: Pattern,
          cont: (Int, Array[Int]) => Int
      ): (Int, Array[Int]) => Int = p match {
        case Pattern.Lit(c) =>
          (pos, groups) =>
            if pos < inputLen && input.charAt(pos) == c.toChar then
              cont(pos + 1, groups)
            else -1

        case Pattern.Class(diet) =>
          (pos, groups) =>
            if pos < inputLen && diet.contains(input.charAt(pos).toInt) then
              cont(pos + 1, groups)
            else -1

        case Pattern.Cat(ps) =>
          ps.foldRight(cont)((sub, acc) => compile(sub, acc))

        case Pattern.Alt(l, r) =>
          val left = compile(l, cont)
          val right = compile(r, cont)
          (pos, groups) => {
            val lp = left(pos, groups)
            if lp >= 0 then lp
            else {
              right(pos, groups)
            }
          }

        case Pattern.Rep0(sub, _) =>
          def loop(pos: Int, groups: Array[Int]): Int = {
            val step = compile(
              sub,
              (nextPos, _) => {
                if nextPos != pos then {
                  val rec = loop(nextPos, groups)
                  if rec >= 0 then rec else -1
                } else -1 // prevent infinite loop
              }
            )

            val out = step(pos, groups)

            if out >= 0 then out
            else {
              cont(pos, groups)
            }
          }

          loop

        case Pattern.Capture(idx, sub) =>
          val inner = compile(
            sub,
            (endPos, _) => {
              val savedEnd = groups(2 * idx + 1)
              groups(2 * idx + 1) = endPos
              val result = cont(endPos, groups)
              if result >= 0 then result
              else {
                groups(2 * idx + 1) = savedEnd
                -1
              }
            }
          )

          (pos, groups) => {
            val savedStart = groups(2 * idx)
            groups(2 * idx) = pos
            val result = inner(pos, groups)
            if result >= 0 then result
            else {
              groups(2 * idx) = savedStart
              -1
            }
          }
      }

      val entryFn = compile(pattern, endCont)
      val matched = entryFn(0, groups)
      if matched >= 0 then
        groups(1) = matched
        Some(groups)
      else None
    }

    matcher
  }

  def matchesWithCaps(
      pattern: Pattern,
      numGroups: Int,
      input: CharSequence
  ): Option[Array[Int]] =
    val matcher = makeMatcher(pattern, numGroups)
    matcher(input, true)

  def matches(
      pattern: Pattern,
      numGroups: Int,
      input: CharSequence
  ): Boolean = {
    val matcher = makeMatcher(pattern, numGroups)
    matcher(input, true).isDefined
  }

  def find(
      pattern: Pattern,
      numGroups: Int,
      input: CharSequence
  ): Boolean = {
    val matcher = makeMatcher(pattern, numGroups)
    val res = matcher(input, false)
    res match
      case Some(groups) =>
        // if we found a match, we need to check if it matches the entire input
        println(s"Found groups: ${groups.mkString(", ")}")
      case None =>
        println("No match found")
    res.isDefined
  }

@main def testCPSRuntime =
  val PatternResult(pattern, groupCount, _, _) =
    Pattern.compile("((a*)b*)*bc|(def)")
  println(
    s"ababc: ${CPSMatcher.matches(pattern, groupCount, "ababc")}"
  ) // true
  println(
    s"aaaaabaababbc: ${CPSMatcher.matches(pattern, groupCount, "aaaaabaababbc")}"
  ) // true
  println(
    s"def: ${CPSMatcher.matches(pattern, groupCount, "def")}"
  ) // true
  // println(CPSMatcher.matches(pattern, groupCount, "xyz")   // false

  println(
    s"bc: ${CPSMatcher.matches(pattern, groupCount, "bc")}"
  ) // true
  // minimal match of first alternative

  println(
    s"abbbbbc: ${CPSMatcher.matches(pattern, groupCount, "abbbbbc")}"
  ) // true
  // a*b* -> a + bbbbb

  println(
    s"abababbbbbc: ${CPSMatcher.matches(pattern, groupCount, "abababbbbbc")}"
  ) // true
  // multiple repetitions of a*b*, followed by bc

  println(s": ${CPSMatcher.matches(pattern, groupCount, "")}") // false
  // empty string doesn't match either alternative

  println(
    s"abc: ${CPSMatcher.matches(pattern, groupCount, "abc")}"
  ) // true
  // a*b* -> a, b, then bc

  println(
    s"defg: ${CPSMatcher.matches(pattern, groupCount, "defg")}"
  ) // false
  // matches "def" but has trailing garbage

  println(
    s"de: ${CPSMatcher.matches(pattern, groupCount, "de")}"
  ) // false
  // partial match on second alternative

  println(
    s"abbbc: ${CPSMatcher.matches(pattern, groupCount, "abbbc")}"
  ) // true

  // test partial matches
  println(
    s"abbbczzz: ${CPSMatcher.find(pattern, groupCount, "abbbczzz")}"
  ) // true
  println(
    s"defg: ${CPSMatcher.find(pattern, groupCount, "defg")}"
  ) // true
  println(
    s"de: ${CPSMatcher.find(pattern, groupCount, "de")}"
  ) // false
