package oregano.internal

import scala.quoted.*

final case class MatchResult(input: CharSequence, matches: Array[Int]) {
  def start(group: Int): Int = matches(2 * group)
  def end(group: Int): Int = matches(2 * group + 1)
  def group(group: Int): String =
    input.subSequence(start(group), end(group)).toString
}

object CPSMatcher:
  def genMatcherPattern(pattern: Pattern, numGroups: Int)(using Quotes): Expr[CharSequence => Boolean] =
    def compile(
      p: Pattern,
      input: Expr[CharSequence],
      matchArray: Expr[Array[Int]],
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
        ps.foldRight(cont)((p, acc) => compile(p, input, matchArray, acc))

      case Pattern.Alt(p1, p2) =>
        val left  = compile(p1, input, matchArray, cont)
        val right = compile(p2, input, matchArray, cont)
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
        val step = compile(sub, input, matchArray, '{ (i: Int) => i })
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
        val inner = compile(sub, input, matchArray, cont)
        '{
          (pos: Int) =>
            val end = $inner(pos)
            if end >= pos then 
              ${matchArray}(${Expr(2 * idx)}) = pos
              ${matchArray}(${Expr(2 * idx + 1)}) = end
            end
        }

    '{
      (input: CharSequence) =>
        val matches = new Array[Int](2 * ${Expr(numGroups)})
        val cont = (i: Int) => {
          if i == input.length then
            matches(1) = i
            i
          else -1
        }
        val matcherFn = ${ compile(pattern, 'input, 'matches, 'cont) }
        matcherFn(0) >= 0
    }