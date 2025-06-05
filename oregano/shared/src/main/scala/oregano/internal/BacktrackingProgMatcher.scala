package oregano.internal

import scala.quoted.*

object BacktrackingProgMatcher:
//   def genMatcher(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =
//     def compile(
//       pc: Int,
//       end: Int,
//       input: Expr[CharSequence],
//       pos: Expr[Int],
//     ): Expr[Int] = pc match
//       case _ if pc == end =>
//         pos

//       case _ =>
//         val inst = prog.getInst(pc)
//         inst.op match

//           case InstOp.MATCH =>
//             '{
//               if $pos == $input.length then
//                 $pos
//               else
//                 -1
//             }

//           case InstOp.FAIL =>
//             '{ -1 }

//           case InstOp.ALT =>
//             val left  = compile(inst.out, end, input, pos)
//             val right = compile(inst.arg, end, input, pos)
//             '{
//               val lp = $left
//               if lp >= 0 then lp
//               else
//                 $right
//             }

//           case InstOp.RUNE | InstOp.RUNE1 =>
//             val runeCheck = inst.matchRuneExpr
//             val nextPos = '{ $pos + 1 }
//             val succ = compile(inst.out, end, input, nextPos)
//             '{
//               if ($pos < $input.length && $runeCheck($input.charAt($pos).toInt))
//                 then
//                   $succ
//                 else -1
//             }

//           case InstOp.LOOP =>
//             val body = (p: Expr[Int]) => compile(inst.out, pc,  input, p)
//             val exit = (p: Expr[Int]) => compile(inst.arg, end, input, p)
//             '{
//               def loop(pos: Int): Int =
//                 val next   = ${ body('{pos}) }
//                 // if no match or zero-length, restore and exit
//                 if next == -1 || next == pos then
//                   ${ exit('{pos}) }
//                 else
//                   val attempt = loop(next)
//                   if attempt >= pos then attempt
//                   else
//                     ${ exit('{pos}) }
//               loop($pos)
//             }

//           case InstOp.CAPTURE =>
//             val nextExp = compile(inst.out, end, input, pos)
//             '{
//               val res     = $nextExp
//               if (res >= 0) then res else -1
//             }

//           case _ =>
//             quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")

//     '{
//       (input: CharSequence) =>
//         val result = ${ compile(prog.start, prog.numInst, 'input, '{0}) }
//         result >= 0
//     }

//   def genMatcherWithCaps(prog: Prog)(using Quotes): Expr[CharSequence => Option[Array[Int]]] =
//     def compile(
//       pc: Int,
//       end: Int,
//       input: Expr[CharSequence],
//       pos: Expr[Int],
//       capCopy: Expr[Array[Int]],
//       cap: Expr[Array[Int]]
//     ): Expr[Int] = pc match
//       case _ if pc == end =>
//         pos

//       case _ =>
//         val inst = prog.getInst(pc)
//         inst.op match

//           case InstOp.MATCH =>
//             '{

//               if $pos == $input.length then
//                 $cap(1) = $pos
//                 $pos
//               else
//                 -1
//             }

//           case InstOp.FAIL =>
//             '{ -1 }

//           case InstOp.ALT =>
//             val left  = compile(inst.out, end, input, pos, capCopy, cap)
//             val right = compile(inst.arg, end, input, pos, capCopy, cap)
//             '{
//               val lp = $left
//               if lp >= 0 then lp
//               else
//                 $right
//             }

//           case InstOp.RUNE | InstOp.RUNE1 =>
//             val runeCheck = inst.matchRuneExpr
//             val nextPos = '{ $pos + 1 }
//             val succ = compile(inst.out, end, input, nextPos, capCopy, cap)
//             '{
//               if ($pos < $input.length && $runeCheck($input.charAt($pos).toInt))
//                 then $succ
//                 else -1
//             }

//           case InstOp.LOOP =>
//             val body = (p: Expr[Int]) => compile(inst.out, pc,  input, p, capCopy, cap)
//             val exit = (p: Expr[Int]) => compile(inst.arg, end, input, p, capCopy, cap)
//             '{
//               def loop(pos: Int): Int =
//                 val next = ${ body('{pos}) }
//                 // if no match or zero-length, restore and exit
//                 if next == -1 || next == pos then
//                   ${ exit('{pos}) }
//                 else
//                   val attempt = loop(next)
//                   if attempt >= pos then attempt
//                   else
//                     ${ exit('{pos}) }

//               loop($pos)
//             }

//           case InstOp.CAPTURE =>
//             val slot = inst.arg
//             val nextExp = compile(inst.out, end, input, pos, capCopy, cap)
//             '{
//               val oldVal = $cap(${Expr(slot)})
//               val curPos = $pos
//               val res = $nextExp

//               if (res >= 0) then
//                 $cap(${Expr(slot)}) = curPos
//                 res
//               else
//                 $cap(${Expr(slot)}) = oldVal
//                 -1
//             }

//           case _ =>
//             quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")

//     '{
//       (input: CharSequence) =>
//         val capCopy = new Array[Int](${Expr(prog.numCap)})
//         val groups = Array.fill(${Expr(prog.numCap)})(-1)
//         groups(0) = 0
//         val result = ${ compile(prog.start, prog.numInst, 'input, '{0}, 'capCopy, 'groups) }

//         if result >= 0 then Some(groups) else None
//     }

  private def compileInst(
      prog: Prog,
      pc: Int,
      end: Int,
      input: Expr[CharSequence],
      pos: Expr[Int],
      withCaps: Boolean,
      capExpr: Expr[Array[Int]],
      wholeMatch: Boolean
  )(using Quotes): Expr[Int] =
    if pc == end then pos
    else
      val inst = prog.getInst(pc)
      inst.op match
        case InstOp.MATCH =>
          if (!withCaps) then
            if wholeMatch then
              '{
                if $pos == $input.length then $pos else -1
              }
            else pos
          else if wholeMatch then
            '{
              if $pos == $input.length then
                $capExpr(1) = $pos
                $pos
              else -1
            }
          else
            '{
              $capExpr(1) = $pos
              $pos
            }

        case InstOp.FAIL =>
          '{ -1 }

        case InstOp.ALT =>
          val leftExpr = compileInst(
            prog,
            inst.out,
            end,
            input,
            pos,
            withCaps,
            capExpr,
            wholeMatch
          )
          val rightExpr = compileInst(
            prog,
            inst.arg,
            end,
            input,
            pos,
            withCaps,
            capExpr,
            wholeMatch
          )
          '{
            val lp = $leftExpr
            if lp >= 0 then lp else $rightExpr
          }

        case InstOp.RUNE | InstOp.RUNE1 =>
          val runeCheck = inst.matchRuneExpr
          val nextPos = '{ $pos + 1 }
          val succExpr = compileInst(
            prog,
            inst.out,
            end,
            input,
            nextPos,
            withCaps,
            capExpr,
            wholeMatch
          )
          '{
            if ($pos < $input.length && $runeCheck($input.charAt($pos).toInt))
            then $succExpr
            else -1
          }

        case InstOp.LOOP =>
          '{
            def loop(posLoop: Int): Int =
              val nextVal = ${
                compileInst(
                  prog,
                  inst.out,
                  pc,
                  input,
                  '{ posLoop },
                  withCaps,
                  capExpr,
                  wholeMatch
                )
              }
              if (nextVal == -1 || nextVal == posLoop) then
                ${
                  compileInst(
                    prog,
                    inst.arg,
                    end,
                    input,
                    '{ posLoop },
                    withCaps,
                    capExpr,
                    wholeMatch
                  )
                }
              else
                val attempt = loop(nextVal)
                if (attempt >= posLoop) then attempt
                else
                  ${
                    compileInst(
                      prog,
                      inst.arg,
                      end,
                      input,
                      '{ posLoop },
                      withCaps,
                      capExpr,
                      wholeMatch
                    )
                  }

            loop($pos)
          }

        case InstOp.CAPTURE =>
          val slot = inst.arg
          val nextExp = compileInst(
            prog,
            inst.out,
            end,
            input,
            pos,
            withCaps,
            capExpr,
            wholeMatch
          )

          if (!withCaps) then
            '{
              val res = $nextExp
              if (res >= 0) then res else -1
            }
          else
            val slotIdx: Expr[Int] = Expr(slot)
            '{
              val oldVal = $capExpr(${ slotIdx })
              val curPos = $pos
              val res = $nextExp
              if (res >= 0) then
                $capExpr(${ slotIdx }) = curPos
                res
              else
                $capExpr(${ slotIdx }) = oldVal
                -1
            }

        case _ =>
          quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")

  def genMatcher(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =
    '{ (input: CharSequence) =>
      val result: Int =
        ${
          compileInst(
            prog,
            prog.start,
            prog.numInst,
            'input,
            '{ 0 },
            false,
            '{ null },
            true
          )
        }
      result == input.length
    }

  def genMatcherWithCaps(prog: Prog)(using
      Quotes
  ): Expr[CharSequence => Option[Array[Int]]] =
    '{ (input: CharSequence) =>
      val groups = Array.fill(${ Expr(prog.numCap) })(-1)
      groups(0) = 0

      val result: Int =
        ${
          compileInst(
            prog,
            prog.start,
            prog.numInst,
            'input,
            '{ 0 },
            true,
            '{ groups },
            true
          )
        }

      if (result == input.length) then Some(groups) else None
    }

  def genFind(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =
    '{ (input: CharSequence) =>
      val groups = Array.fill(${ Expr(prog.numCap) })(-1)
      groups(0) = 0

      val result: Int =
        ${
          compileInst(
            prog,
            prog.start,
            prog.numInst,
            'input,
            '{ 0 },
            false,
            '{ null },
            false
          )
        }

      result >= 0
    }

  def matches(prog: Prog, input: CharSequence): Boolean = {
    val cap = new Array[Int](prog.numCap)

    def compile(pc: Int, end: Int, pos: Int): Int = {
      if (pc == end) return pos

      val inst = prog.getInst(pc)

      inst.op match {
        case InstOp.MATCH =>
          if (pos == input.length) {
            cap(1) = pos // Capture full match end
            pos
          } else -1

        case InstOp.FAIL =>
          -1

        case InstOp.ALT =>
          val save = cap.clone()
          val left = compile(inst.out, end, pos)
          if (left >= 0) left
          else {
            Array.copy(save, 0, cap, 0, cap.length)
            compile(inst.arg, end, pos)
          }

        case InstOp.RUNE | InstOp.RUNE1 =>
          if (pos < input.length && inst.matchRune(input.charAt(pos).toInt))
            compile(inst.out, end, pos + 1)
          else -1

        case InstOp.LOOP =>
          def loop(p: Int): Int = {
            val save = cap.clone()
            val next = compile(inst.out, pc, p)
            if (next >= 0 && next != p) {
              val inner = loop(next)
              if (inner >= 0) inner
              else {
                Array.copy(save, 0, cap, 0, cap.length)
                compile(inst.arg, end, p)
              }
            } else {
              compile(inst.arg, end, p)
            }
          }
          loop(pos)

        case InstOp.CAPTURE =>
          val slot = inst.arg
          val nextPC = inst.out
          if (slot % 2 == 0) {
            val close = slot + 1
            val oldStart = cap(slot)
            val oldEnd = cap(close)
            cap(slot) = pos
            val result = compile(nextPC, end, pos)
            if (result >= 0) {
              cap(close) = result
              result
            } else {
              cap(slot) = oldStart
              cap(close) = oldEnd
              -1
            }
          } else {
            val old = cap(slot)
            cap(slot) = pos
            val result = compile(nextPC, end, pos)
            if (result < 0) cap(slot) = old
            result
          }

        case _ =>
          throw new RuntimeException(s"Unsupported op: ${inst.op}")
      }
    }

    val result = compile(prog.start, prog.numInst, 0)
    if (result >= 0) {
      println(cap.mkString(" "))
      true
    } else false
  }

@main def testProgMatcher(): Unit =
  val PatternResult(pat, numGroups, _, _) = Pattern.compile("(a*b*)*bc")
  val prog = ProgramCompiler.compileRegexp(pat, numGroups)
  println("prog: " + BacktrackingProgMatcher.matches(prog, "abababc"))
