package oregano.internal

import scala.quoted.*
import cats.collections.Diet

final case class MatchResult(input: CharSequence, matches: Array[Int]) {
  def start(group: Int): Int = matches(2 * group)
  def end(group: Int): Int = matches(2 * group + 1)
  def group(group: Int): String =
    input.subSequence(start(group), end(group)).toString
}

// essentially copy matchRuneExpr; todo: use a common function?
def dietContains(diet: Diet[Int])(using Quotes): Expr[Int => Boolean] = {
  val runes: List[Int] = Utils.dietToRanges(diet)

  val pairs: List[(Int, Int)] = runes
    .grouped(2)
    .collect { case List(lo, hi) =>
      (lo, hi)
    }
    .toList

  // vaccuously handle diet with 1 rune, by construction singleton does not exist
  '{ (r: Int) =>
    ${
      val conditions = pairs.map { case (lo, hi) =>
        if lo == hi then '{ r == ${ Expr(lo) } }
        else '{ r >= ${ Expr(lo) } && r <= ${ Expr(hi) } }
      }
      conditions.reduceLeft((a, b) => '{ $a || $b })
    }
  }
}

// object CPSMatcher:
//   def genMatcherPattern(pattern: Pattern)(using Quotes): Expr[CharSequence => Boolean] =
//     def compile(
//       p: Pattern,
//       input: Expr[CharSequence],
//       cont: Expr[Int => Int]
//     ): Expr[Int => Int] = p match
//       case Pattern.Lit(c) =>
//         '{
//           (pos: Int) =>
//             if pos < $input.length && $input.charAt(pos) == ${Expr(c.toChar)} then
//               $cont(pos + 1)
//             else -1
//         }

//       case Pattern.Cat(ps) =>
//         ps.foldRight(cont)((p, acc) => compile(p, input, acc))

//       case Pattern.Alt(p1, p2) =>
//         val left  = compile(p1, input, cont)
//         val right = compile(p2, input, cont)
//         '{
//           (pos: Int) =>
//             val lp = $left(pos)
//             if lp >= 0 then lp else $right(pos)
//         }

//       case Pattern.Class(diet) =>
//         val runeCheckExpr = dietContains(diet)
//         '{
//           (pos: Int) =>
//             if pos < $input.length && ${Expr.betaReduce('{ $runeCheckExpr($input.charAt(pos).toInt) })} then
//               $cont(pos + 1)
//             else -1
//         }

//       case Pattern.Rep0(sub, _) =>
//         '{
//           def self(pos: Int): Int =
//             val step = ${
//               compile(sub, input, '{
//                 (next: Int) =>
//                   if next != pos then self(next) else -1
//               })
//             }
//             val r = step(pos)
//             if r >= 0 then r else ${ cont }(pos)
//           self
//         }

//       case Pattern.Capture(idx, sub) =>
//         val inner = compile(sub, input, cont)
//         '{
//           (pos: Int) =>
//             $inner(pos)
//         }

//     '{
//       (input: CharSequence) =>
//         val cont = (i: Int) => {
//           if i == input.length then
//             i
//           else -1
//         }
//         val matcherFn = ${ compile(pattern, 'input, 'cont) }
//         if matcherFn(0) >= 0 then
//           true
//         else false
//     }

//   // using explicit call-stack tracking a la java.util.regex
//   def genMatcherPatternWithCaps(pattern: Pattern, numGroups: Int)(using Quotes): Expr[CharSequence => Option[Array[Int]]] =
//     def compile(
//       p: Pattern,
//       input: Expr[CharSequence],
//       groups: Expr[Array[Int]],
//       cont: Expr[Int => Int]
//     ): Expr[Int => Int] = p match
//       case Pattern.Lit(c) =>
//         '{
//           (pos: Int) =>
//             if pos < $input.length && $input.charAt(pos) == ${Expr(c.toChar)} then
//               $cont(pos + 1)
//             else -1
//         }

//       case Pattern.Class(diet) =>
//         val runeCheckExpr = dietContains(diet)
//         '{
//           (pos: Int) =>
//             if pos < $input.length && ${Expr.betaReduce('{ $runeCheckExpr($input.charAt(pos).toInt) })} then
//               $cont(pos + 1)
//             else -1
//         }

//       case Pattern.Cat(ps) =>
//         ps.foldRight(cont)((sub, acc) => compile(sub, input, groups, acc))

//       case Pattern.Alt(l, r) =>
//         val left  = compile(l, input, groups, cont)
//         val right = compile(r, input, groups, cont)
//         '{
//           (pos: Int) =>
//             val lp = $left(pos)
//             if lp >= 0 then lp
//             else {
//               $right(pos)
//             }
//         }

//       case Pattern.Rep0(sub, _) =>
//         '{
//           def self(pos: Int): Int =
//             val step = ${
//               compile(sub, input, groups, '{
//                 (next: Int) =>
//                   if next != pos then self(next) else -1
//               })
//             }
//             val out = step(pos)
//             if out >= 0 then out
//             else {
//               $cont(pos)
//             }
//           self
//         }

//       case Pattern.Capture(idx, sub) =>
//         val inner = compile(sub, input, groups, '{
//           (endPos: Int) =>
//             val savedEnd = $groups(2 * ${Expr(idx)} + 1)
//             $groups(2 * ${Expr(idx)} + 1) = endPos
//             val res = $cont(endPos)
//             if res >= 0 then res
//             else {
//               $groups(2 * ${Expr(idx)} + 1) = savedEnd
//               -1
//             }
//         })
//         '{
//           (pos: Int) =>
//             val savedStart = $groups(2 * ${Expr(idx)})
//             $groups(2 * ${Expr(idx)}) = pos
//             val res = $inner(pos)
//             if res >= 0 then res
//             else {
//               $groups(2 * ${Expr(idx)}) = savedStart
//               -1
//             }
//         }

//     '{
//       (input: CharSequence) =>
//         val inputLen = input.length
//         val groups = Array.fill(${Expr(numGroups * 2)})(-1)
//         groups(0) = 0

//         val cont = (i: Int) =>
//           if i == inputLen then i else -1

//         val matcherFn = ${ compile(pattern, 'input, 'groups, 'cont) }

//         val matched = matcherFn(0)
//         if matched >= 0 then {
//           groups(1) = matched // end of match
//           // println(s"Success: ${groups.mkString(", ")}")
//           Some(groups)
//         } else None
//     }

object CPSMatcher:
  private def compile(
      p: Pattern,
      input: Expr[CharSequence],
      noCaps: Int, 
      cont: Expr[Int => Int],
      withCaps: Boolean,
      groupsExpr: Expr[Array[Int]]
  )(using Quotes): Expr[Int => Int] =

    p match
      case Pattern.Lit(c) =>
        '{ (pos: Int) =>
          if pos < $input.length && $input.charAt(pos) == ${ Expr(c) } then
            $cont(pos + 1)
          else -1
        }

      case Pattern.Class(diet) =>
        val runeCheck: Expr[Int => Boolean] = dietContains(diet)
        '{ (pos: Int) =>
          if pos < $input.length &&
            ${ Expr.betaReduce('{ $runeCheck($input.charAt(pos).toInt) }) }
          then $cont(pos + 1)
          else -1
        }

      case Pattern.Cat(ps) =>
        ps.foldRight(cont) { (subPattern, nextCont) =>
          compile(subPattern, input, noCaps, nextCont, withCaps, groupsExpr)
        }

      case Pattern.Alt(p1, p2) =>
        val leftExpr = compile(p1, input, noCaps, cont, withCaps, groupsExpr)
        val rightExpr = compile(p2, input, noCaps, cont, withCaps, groupsExpr)
        '{ (pos: Int) =>
          val lp = $leftExpr(pos)
          if lp >= 0 then lp else $rightExpr(pos)
        }

      case Pattern.Rep0(sub, _) =>
        '{
          def self(pos: Int): Int =
            val step = ${
              compile(
                sub,
                input,
                noCaps,
                '{ (next: Int) =>
                  if next != pos then self(next) else -1
                },
                withCaps,
                groupsExpr
              )
            }
            val r = step(pos)
            if r >= 0 then r else $cont(pos)
          self
        }

      case Pattern.Capture(idx, sub) =>
        if (!withCaps || idx > noCaps) then 
          compile(sub, input, noCaps, cont, false, '{ null })
        else
          val endSlotIdx: Expr[Int] = Expr(2 * idx + 1)
          val cont1: Expr[Int => Int] =
            '{ (endPos: Int) =>
              val savedEnd = $groupsExpr(${ endSlotIdx })
              $groupsExpr(${ endSlotIdx }) = endPos
              val res = $cont(endPos)
              if (res >= 0) then res
              else {
                $groupsExpr(${ endSlotIdx }) = savedEnd
                -1
              }
            }

          val inner: Expr[Int => Int] =
            compile(sub, input, noCaps, cont1, true, groupsExpr)

          val startSlotIdx: Expr[Int] = Expr(2 * idx)
          '{ (pos: Int) =>
            val savedStart = $groupsExpr(${ startSlotIdx })
            $groupsExpr(${ startSlotIdx }) = pos
            val r2 = ${ inner }(pos)
            if (r2 >= 0) then r2
            else {
              $groupsExpr(${ startSlotIdx }) = savedStart
              -1
            }
          }

  def genMatcherPattern(pattern: Pattern)(using
      Quotes
  ): Expr[CharSequence => Boolean] =
    '{ (input: CharSequence) =>
      val cont: Int => Int = (i: Int) => if i == input.length then i else -1

      val matcherFn: Int => Int =
        ${ compile(pattern, 'input, 0, '{ cont }, false, '{ null }) }

      matcherFn(0) == input.length
    }

  def genMatcherPatternWithCaps(pattern: Pattern, numGroups: Int)(using
      Quotes
  ): Expr[CharSequence => Option[Array[Int]]] =
    '{ (input: CharSequence) =>
      val inputLen = input.length
      val groups = Array.fill(${ Expr(numGroups * 2) })(-1)
      groups(0) = 0

      val cont: Int => Int = (i: Int) => if i == inputLen then i else -1

      val matcherFn: Int => Int =
        ${ compile(pattern, 'input, numGroups, '{ cont }, true, '{ groups }) }

      val matched = matcherFn(0)
      if matched == inputLen then
        groups(1) = matched
        Some(groups)
      else None
    }

  def genFinderPattern(pattern: Pattern, numGroups: Int)(using
      Quotes
  ): Expr[CharSequence => Boolean] =
    '{ (input: CharSequence) =>
      val matcherFn: Int => Int =
        ${
          compile(pattern, 'input, 0, '{ (i: Int) => i }, false, '{ null })
        }

      matcherFn(0) >= 0
    }

  def makeMatcher(
      pattern: Pattern,
      numGroups: Int,
      numReps: Int
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
      numReps: Int,
      input: CharSequence
  ): Option[Array[Int]] =
    val matcher = makeMatcher(pattern, numGroups, numReps)
    matcher(input, true)

  def matches(
      pattern: Pattern,
      numGroups: Int,
      numReps: Int,
      input: CharSequence
  ): Boolean = {
    val matcher = makeMatcher(pattern, numGroups, numReps)
    matcher(input, true).isDefined
  }

  def find(
      pattern: Pattern,
      numGroups: Int,
      numReps: Int,
      input: CharSequence
  ): Boolean = {
    val matcher = makeMatcher(pattern, numGroups, numReps)
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
  val PatternResult(pattern, groupCount, _, numReps) =
    Pattern.compile("((a*)b*)*bc|(def)")
  println(
    s"ababc: ${CPSMatcher.matches(pattern, groupCount, numReps, "ababc")}"
  ) // true
  println(
    s"aaaaabaababbc: ${CPSMatcher.matches(pattern, groupCount, numReps, "aaaaabaababbc")}"
  ) // true
  println(
    s"def: ${CPSMatcher.matches(pattern, groupCount, numReps, "def")}"
  ) // true
  // println(CPSMatcher.matches(pattern, groupCount, "xyz")   // false

  println(
    s"bc: ${CPSMatcher.matches(pattern, groupCount, numReps, "bc")}"
  ) // true
  // minimal match of first alternative

  println(
    s"abbbbbc: ${CPSMatcher.matches(pattern, groupCount, numReps, "abbbbbc")}"
  ) // true
  // a*b* -> a + bbbbb

  println(
    s"abababbbbbc: ${CPSMatcher.matches(pattern, groupCount, numReps, "abababbbbbc")}"
  ) // true
  // multiple repetitions of a*b*, followed by bc

  println(s": ${CPSMatcher.matches(pattern, groupCount, numReps, "")}") // false
  // empty string doesn't match either alternative

  println(
    s"abc: ${CPSMatcher.matches(pattern, groupCount, numReps, "abc")}"
  ) // true
  // a*b* -> a, b, then bc

  println(
    s"defg: ${CPSMatcher.matches(pattern, groupCount, numReps, "defg")}"
  ) // false
  // matches "def" but has trailing garbage

  println(
    s"de: ${CPSMatcher.matches(pattern, groupCount, numReps, "de")}"
  ) // false
  // partial match on second alternative

  println(
    s"abbbc: ${CPSMatcher.matches(pattern, groupCount, numReps, "abbbc")}"
  ) // true

  // test partial matches
  println(
    s"abbbczzz: ${CPSMatcher.find(pattern, groupCount, numReps, "abbbczzz")}"
  ) // true
  println(
    s"defg: ${CPSMatcher.find(pattern, groupCount, numReps, "defg")}"
  ) // true
  println(
    s"de: ${CPSMatcher.find(pattern, groupCount, numReps, "de")}"
  ) // false
