package oregano.internal

import scala.quoted.*
import cats.collections.Diet
import scala.collection.mutable.ArrayBuffer

final case class MatchResult(input: CharSequence, matches: Array[Int]) {
  def start(group: Int): Int = matches(2 * group)
  def end(group: Int): Int = matches(2 * group + 1)
  def group(group: Int): String =
    input.subSequence(start(group), end(group)).toString
}

class CheckpointStack(groups: Array[Int]) {
  val stack = ArrayBuffer[(Int, Int)]()

  inline def saveState(pos: Int): Unit = 
    stack += ((pos, groups(pos)))

  inline def checkpoint(): Int = stack.size

  inline def restore(checkpoint: Int): Unit = {
    while (stack.size > checkpoint) {
      val (pos, oldVal) = stack.remove(stack.size - 1)
      groups(pos) = oldVal
    }
  }
}

// essentially copy matchRuneExpr; todo: use a common function?
def dietContains(diet: Diet[Int])(using Quotes): Expr[Int => Boolean] = {
  val runes: List[Int] = dietToRanges(diet)

  val pairs: List[(Int, Int)] = runes.grouped(2).collect {
    case List(lo, hi) => (lo, hi)
  }.toList

  // vaccuously handle diet with 1 rune
  if pairs.length <= 4 then
    '{
      (r: Int) => ${
        val conditions = pairs.map { case (lo, hi) =>
          if lo == hi then
            '{ r == ${Expr(lo)} }
          else
            '{ r >= ${Expr(lo)} && r <= ${Expr(hi)} }
        }
        conditions.reduceLeft((a, b) => '{ $a || $b })
      }
    }

  else
    val pairsArgsExpr = Varargs(
      pairs.map { 
        case (lo, hi) => Expr.ofTuple((Expr(lo), Expr(hi))) 
      }
    )
    '{
      val pairs = IArray($pairsArgsExpr*)
      (r: Int) =>
        var ret = false
        var lo = 0
        var hi = pairs.length
        while lo < hi do
          val m = lo + (hi - lo) / 2
          val (rlo, rhi) = pairs(m)
          if r < rlo then hi = m
          else if r > rhi then lo = m + 1
          else {
            ret = true
            lo = hi
          }
        ret
    }
}


object CPSMatcher:
  def genMatcherPattern(pattern: Pattern)(using Quotes): Expr[CharSequence => Boolean] =
    def compile(
      p: Pattern,
      input: Expr[CharSequence],
      cont: Expr[Int => Int]
    ): Expr[Int => Int] = p match
      case Pattern.Lit(c) =>
        '{
          (pos: Int) =>
            if pos < $input.length && $input.charAt(pos) == ${Expr(c.toChar)} then
              $cont(pos + 1)
            else -1
        }

      case Pattern.Cat(ps) =>
        ps.foldRight(cont)((p, acc) => compile(p, input, acc))

      case Pattern.Alt(p1, p2) =>
        val left  = compile(p1, input, cont)
        val right = compile(p2, input, cont)
        '{
          (pos: Int) =>
            val lp = $left(pos)
            if lp >= 0 then lp else $right(pos)
        }

      case Pattern.Class(diet) =>
        val runeCheckExpr = dietContains(diet)
        '{
          (pos: Int) =>
            if pos < $input.length && ${Expr.betaReduce('{ $runeCheckExpr($input.charAt(pos).toInt) })} then
              $cont(pos + 1)
            else -1
        }

      case Pattern.Rep0(sub, _) =>
        '{
          def self(pos: Int): Int =
            val step = ${
              compile(sub, input, '{
                (next: Int) =>
                  if next != pos then self(next) else -1
              })
            }
            val r = step(pos)
            if r >= 0 then r else ${ cont }(pos)
          self
        }

      case Pattern.Capture(idx, sub) =>
        val inner = compile(sub, input, cont)
        '{
          (pos: Int) =>
            $inner(pos)
        }

    '{
      (input: CharSequence) =>
        val cont = (i: Int) => {
          if i == input.length then
            i
          else -1
        }
        val matcherFn = ${ compile(pattern, 'input, 'cont) }
        if matcherFn(0) >= 0 then
          true
        else false
    }

  // def makeMatcher(pattern: Pattern, numGroups: Int): CharSequence => Boolean =
  //   (input: CharSequence) => {
  //     val inputLen = input.length
  //     val contArr  = new Array[Int](2 * numGroups)

  //     val endCont: Int => Int = i => if (i == inputLen) then
  //         contArr(1) = i
  //         i 
  //       else -1

  //     def compile(p: Pattern, cont: Int => Int): Int => Int = p match {

  //       case Pattern.Lit(c) =>
  //         (pos: Int) =>
  //           if pos < inputLen && input.charAt(pos) == c.toChar then
  //             cont(pos + 1)
  //           else
  //             -1

  //       case Pattern.Cat(ps) =>
  //         // foldRight threads the continuation through the list
  //         ps.foldRight(cont)((sub, acc) => compile(sub, acc))

  //       case Pattern.Alt(l, r) =>
  //         val left  = compile(l, cont)
  //         val right = compile(r, cont)
  //         (pos: Int) =>
  //           val lp = left(pos)
  //           if lp >= 0 then lp else right(pos)

  //       case Pattern.Class(diet) =>
  //         (pos: Int) =>
  //           if pos < inputLen && diet.contains(input.charAt(pos).toInt) then
  //             cont(pos + 1)
  //           else
  //             -1

  //       case Pattern.Rep0(sub) =>
  //         val step = compile(sub, i => i)
  //         lazy val self: Int => Int = (pos: Int) => {
  //           val nxt = step(pos)
  //           if nxt >= 0 && nxt != pos then
  //             val r = self(nxt)
  //             if r >= 0 then r else cont(pos)
  //           else
  //             cont(pos)
  //         }
  //         self

  //       case Pattern.Capture(idx, sub) =>
  //         val inner = compile(sub, cont)
  //         (pos: Int) => {
  //           val end = inner(pos)
  //           if end >= pos then
  //             contArr(2 * idx) = pos
  //             contArr(2 * idx + 1) = end
  //           end
  //         }
  //     }

  //     val entryFn: Int => Int = compile(pattern, endCont)
  //     // DEBUG
  //     // val result = entryFn(0)
  //     // if result >= 0 then 
  //     //   println(contArr.mkString(" "))
  //     // result >= 0
  //     entryFn(0) >= 0
  //   }

  // def makeMatcher(pattern: Pattern, numGroups: Int, numReps: Int): CharSequence => Boolean = {
  //   def matcher(input: CharSequence): Boolean = {
  //     val inputLen = input.length
  //     val groups: Array[Int] = Array.fill(numGroups * 2)(-1)
  //     // val groupScratch: Array[Array[Int]] = Array.fill(numReps)(Array.fill(numGroups * 2)(-1))
  //     groups(0) = 0

  //     val endCont: (Int, Array[Int]) => Int = (i, _) =>
  //       if i == inputLen then 
  //         println(s"MATCHED: ${groups.mkString(" ")}")
  //         i 
  //         else -1

  //     def compile(p: Pattern, cont: (Int, Array[Int]) => Int): (Int, Array[Int]) => Int = p match {
  //       case Pattern.Lit(c) =>
  //         (pos, groups) =>
  //           if pos < inputLen && input.charAt(pos) == c.toChar then
  //             cont(pos + 1, groups)
  //           else
  //             -1

  //       case Pattern.Class(diet) =>
  //         (pos, groups) =>
  //           if pos < inputLen && diet.contains(input.charAt(pos).toInt) then
  //             cont(pos + 1, groups)
  //           else
  //             -1

  //       case Pattern.Cat(ps) =>
  //         ps.foldRight(cont)((sub, acc) => compile(sub, acc))

  //       case Pattern.Alt(l, r) =>
  //         val left = compile(l, cont)
  //         val right = compile(r, cont)
  //         (pos, groups) => {
  //           val save = groups.clone()
  //           val lp = left(pos, groups)
  //           if lp >= 0 then lp
  //           else right(pos, save)
  //         }

  //       case Pattern.Rep0(sub, scratchIdx) =>
  //         def loop(pos: Int, groups: Array[Int]): Int = {
  //           // val scratch = groupScratch(scratchIdx)
  //           // Array.copy(groups, 0, scratch, 0, groups.length)
  //           val scratch = groups.clone()
  //           println(s"loop($scratchIdx, ${scratch.mkString(",")})")

  //           val step = compile(sub, (nextPos, innerGroups) => {
  //             if nextPos != pos then {
  //               val rec = loop(nextPos, innerGroups)
  //               if rec >= 0 then {
  //                 Array.copy(innerGroups, 0, groups, 0, groups.length)
  //                 println(s"copying back, $scratchIdx ${innerGroups.mkString(",")}")
  //                 rec
  //               } else -1
  //             } else -1 // prevent infinite loop on nullable
  //           })

  //           val out = step(pos, scratch)

  //           if out >= 0 then out
  //           else cont(pos, groups)
  //         }

  //         loop

  //       case Pattern.Capture(idx, sub) =>
  //         val inner = compile(sub, (endPos, g) => {
  //           g(2 * idx + 1) = endPos 
  //           cont(endPos, g)
  //         })

  //         (pos, groups) => {
  //           groups(2 * idx) = pos
  //           inner(pos, groups)
  //         }
  //     }

  //     val entryFn = compile(pattern, endCont)
  //     val matched = entryFn(0, groups)
  //     if matched >= 0 then
  //       groups(1) = matched
  //     println(groups.mkString(" "))
  //     matched >= 0
  //   }

  //   matcher
  // }

  def makeMatcher(pattern: Pattern, numGroups: Int, numReps: Int): CharSequence => Boolean = {
    def matcher(input: CharSequence): Boolean = {
      val inputLen = input.length
      val groups: Array[Int] = Array.fill(numGroups * 2)(-1)
      groups(0) = 0

      val stack = new CheckpointStack(groups)

      val endCont: (Int, Array[Int]) => Int = (i, _) =>
        if i == inputLen then i else -1

      def compile(p: Pattern, cont: (Int, Array[Int]) => Int): (Int, Array[Int]) => Int = p match {
        case Pattern.Lit(c) =>
          (pos, groups) =>
            if pos < inputLen && input.charAt(pos) == c.toChar then cont(pos + 1, groups)
            else -1

        case Pattern.Class(diet) =>
          (pos, groups) =>
            if pos < inputLen && diet.contains(input.charAt(pos).toInt) then cont(pos + 1, groups)
            else -1

        case Pattern.Cat(ps) =>
          ps.foldRight(cont)((sub, acc) => compile(sub, acc))

        case Pattern.Alt(l, r) =>
          val left = compile(l, cont)
          val right = compile(r, cont)
          (pos, groups) => {
            val cp = stack.checkpoint()
            val lp = left(pos, groups)
            if lp >= 0 then lp
            else {
              stack.restore(cp)
              right(pos, groups)
            }
          }

        case Pattern.Rep0(sub, _) =>
          def loop(pos: Int, groups: Array[Int]): Int = {
            val cp = stack.checkpoint()

            val step = compile(sub, (nextPos, _) => {
              if nextPos != pos then {
                val rec = loop(nextPos, groups)
                if rec >= 0 then rec else -1
              } else -1 // prevent infinite loop
            })

            val out = step(pos, groups)

            if out >= 0 then out
            else {
              stack.restore(cp)
              cont(pos, groups)
            }
          }

          loop

        case Pattern.Capture(idx, sub) =>
          val inner = compile(sub, (endPos, _) => {
            stack.saveState(2 * idx + 1)
            groups(2 * idx + 1) = endPos
            cont(endPos, groups)
          })

          (pos, groups) => {
            stack.saveState(2 * idx)
            groups(2 * idx) = pos
            inner(pos, groups)
          }
      }

      val entryFn = compile(pattern, endCont)
      val matched = entryFn(0, groups)
      if matched >= 0 then groups(1) = matched
      println(groups.mkString(" "))
      matched >= 0
    }

    matcher
  }


  def matches(pattern: Pattern, numGroups: Int, numReps: Int, input: CharSequence): Boolean = {
    val matcher = makeMatcher(pattern, numGroups, numReps)
    matcher(input)
  }


@main def testCPSRuntime =
  val PatternResult(pattern, groupCount, _, numReps) = Pattern.compile("(a*b*)*bc|(def)")
  println(s"ababc: ${CPSMatcher.matches(pattern, groupCount, numReps,  "ababc")}") // true
  println(s"aaaaabaababc: ${CPSMatcher.matches(pattern, groupCount, numReps, "aaaaabaababc")}")   // true
  println(s"def: ${CPSMatcher.matches(pattern, groupCount, numReps, "def")}")   // true
  // println(CPSMatcher.matches(pattern, groupCount, "xyz"))   // false