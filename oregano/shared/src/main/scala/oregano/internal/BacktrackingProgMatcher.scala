package oregano.internal

import scala.quoted.*

object BacktrackingProgMatcher:
// Might be useful keeping this for profiling reasons, obviously 
// better to 'bake in' Exprs and avoid runtime closure overhead
// def genMatcher(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =
//   def compile(pc: Int, end: Int, input: Expr[CharSequence]): Expr[Int => Int] =
//     if pc == end then
//       return '{ (pos: Int) => pos }
//     val inst = prog.getInst(pc)
//     inst.op match
//       case InstOp.MATCH =>
//         '{
//           (pos: Int) =>
//             if pos == $input.length then pos else -1
//         }

//       case InstOp.FAIL =>
//         '{ (_: Int) => -1 }

//       case InstOp.ALT =>
//         lazy val left = compile(inst.out, end, input)
//         lazy val right = compile(inst.arg, end, input)
//         '{
//           (pos: Int) =>
//             lazy val lp = $left(pos)
//             if lp >= 0 then lp else $right(pos)
//         }

//       case InstOp.RUNE | InstOp.RUNE1 =>
//         val runeCheck = inst.matchRuneExpr
//         val outExpr = compile(inst.out, end, input)
//         '{
//           (pos: Int) =>
//             if pos < $input.length && $runeCheck($input.charAt(pos).toInt)
//               then $outExpr(pos + 1)
//               else -1
//         }
      
//       case InstOp.LOOP =>
//         // will need to be careful with lazy */+
//         val loopStart = inst.out
//         val loopEnd   = pc
//         val bodyExpr  = compile(loopStart, loopEnd, input)
//         val exitExpr  = compile(inst.arg, end, input)
//         '{
//           def loop(pos: Int): Int =
//             val next = $bodyExpr(pos)
//             if next >= 0 && next != pos then
//               val attempt = loop(next) 
//               if attempt >= 0 then attempt
//               else $exitExpr(pos)
//             else $exitExpr(pos)

//           loop
//         }

//       case _ =>
//         quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")

//   '{
//     (input: CharSequence) =>
//       val matcher = ${ compile(prog.start, prog.numInst, 'input) }
//       matcher(0) >= 0
//   }

  def genMatcher(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =
    def compile(pc: Int, end: Int, input: Expr[CharSequence], pos: Expr[Int]): Expr[Int] = pc match
      case _ if pc == end =>
        pos

      case _ =>
        val inst = prog.getInst(pc)
        inst.op match
          case InstOp.MATCH =>
            '{ if $pos == $input.length then $pos else -1 }

          case InstOp.FAIL =>
            '{ -1 }

          case InstOp.ALT =>
            val left = compile(inst.out, end, input, pos)
            val right = compile(inst.arg, end, input, pos)
            '{ val lp = $left; if lp >= 0 then lp else $right }

          case InstOp.RUNE | InstOp.RUNE1 =>
            val runeCheck = inst.matchRuneExpr
            val nextPos = '{ $pos + 1 }
            val success = compile(inst.out, end, input, nextPos)
            '{ 
              if $pos < $input.length && $runeCheck($input.charAt($pos).toInt)
                then $success
                else -1
            }

          case InstOp.LOOP =>
            val body = (p: Expr[Int]) => compile(inst.out, pc, input, p)
            val exit = (p: Expr[Int]) => compile(inst.arg, end, input, p)

            '{ 
              def loop(pos: Int): Int =
                val next = ${ body('pos) }
                if next >= 0 && next != pos then
                  val attempt = loop(next)
                  if attempt >= 0 then attempt
                  else ${ exit('pos) }
                else ${ exit('pos) }

              loop($pos)
            }

          case _ =>
            quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")

    '{
      (input: CharSequence) =>
        val result = ${ compile(prog.start, prog.numInst, 'input, '{0}) }
        result >= 0
    }

  def matches(prog: Prog, input: CharSequence): Boolean = {
    def compile(pc: Int, end: Int, input: CharSequence, pos: Int): Int = {
      if (pc == end) return pos

      val inst = prog.getInst(pc)

      inst.op match {
        case InstOp.MATCH =>
          if (pos == input.length) pos else -1

        case InstOp.FAIL =>
          -1

        case InstOp.ALT =>
          val left = compile(inst.out, end, input, pos)
          if (left >= 0) left else compile(inst.arg, end, input, pos)

        case InstOp.RUNE | InstOp.RUNE1 =>
          if (pos < input.length && inst.matchRune(input.charAt(pos).toInt))
            compile(inst.out, end, input, pos + 1)
          else -1

        case InstOp.LOOP =>
          def loop(pos0: Int): Int = {
            val next = compile(inst.out, pc, input, pos0)
            if (next >= 0 && next != pos0) {
              val attempt = loop(next)
              if (attempt >= 0) attempt
              else compile(inst.arg, end, input, pos0)
            } else {
              compile(inst.arg, end, input, pos0)
            }
          }
          loop(pos)

        case _ =>
          throw new RuntimeException(s"Unsupported op: ${inst.op}")
      }
    }

    compile(prog.start, prog.numInst, input, 0) >= 0
  }
