package oregano.internal

import scala.quoted.*
import oregano.internal.Regex.Digit.con

final case class MatchResult(input: CharSequence, matches: Array[Int]) {
  def start(group: Int): Int = matches(2 * group)
  def end(group: Int): Int = matches(2 * group + 1)
  def group(group: Int): String =
    input.subSequence(start(group), end(group)).toString
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
        '{
          (pos: Int) =>
            if pos < $input.length && ${Expr(diet)}.contains($input.charAt(pos).toInt) then
              $cont(pos + 1)
            else -1
        }

      case Pattern.Rep0(sub) =>
        val step = compile(sub, input, '{ (i: Int) => i })
        '{
          val stepFn: Int => Int = $step
          lazy val self: Int => Int = (pos: Int) =>
            val next = stepFn(pos)
            if next >= 0 && next != pos then
              val r = self(next)
              if r >= 0 then r else $cont(pos)
            else $cont(pos)
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

  def makeMatcher(pattern: Pattern, numGroups: Int): (CharSequence => Boolean) = {
    def matcher(input: CharSequence): Boolean = {
      val inputLen = input.length

      val endCont: Int => Int = i =>
        if i == inputLen then
          i
        else -1

      def compile(p: Pattern, cont: Int => Int): Int => Int = p match {
        case Pattern.Lit(c) =>
          (pos: Int) =>
            if pos < inputLen && input.charAt(pos) == c.toChar then
              cont(pos + 1)
            else
              -1

        case Pattern.Cat(ps) =>
          ps.foldRight(cont)((sub, acc) => compile(sub, acc))

        case Pattern.Alt(l, r) =>
          val left  = compile(l, cont)
          val right = compile(r, cont)
          (pos: Int) =>
            val lp = left(pos)
            if lp >= 0 then lp else right(pos)

        case Pattern.Class(diet) =>
          (pos: Int) =>
            if pos < inputLen && diet.contains(input.charAt(pos).toInt) then
              cont(pos + 1)
            else
              -1

       case Pattern.Rep0(sub) =>
          def self(pos: Int): Int = {
            val step = compile(sub, (nextPos: Int) =>
              if nextPos != pos then self(nextPos)
              else -1
            )

            val out = step(pos)
            if out >= 0 then out else cont(pos)
          }
          self

        case Pattern.Capture(idx, sub) =>
          val inner = compile(sub, cont)
          (pos: Int) => {
            val end = inner(pos)
            end
          }
      }

      val entryFn = compile(pattern, endCont)
      entryFn(0) >= 0
    }

    matcher
  }

  def matches(pattern: Pattern, numGroups: Int, input: CharSequence): Boolean = {
    val matcher = makeMatcher(pattern, numGroups)
    matcher(input)
  }


@main def testCPSRuntime =
  val (pattern, groupCount) = Pattern.compile("(a*b*)*bc|def")
  println(CPSMatcher.matches(pattern, groupCount, "abababc")) // true
  println(CPSMatcher.matches(pattern, groupCount, "def"))   // true
  println(CPSMatcher.matches(pattern, groupCount, "xyz"))   // false