package oregano.internal

import scala.quoted.*

object BacktrackingProgMatcher:
// Might be useful keeping this for profiling reasons, obviously 
// better to 'bake in' Exprs and avoid runtime closure overhead
  def genMatcherLambda(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =
    def compile(pc: Int, end: Int, input: Expr[CharSequence]): Expr[Int => Int] =
      if pc == end then
        return '{ (pos: Int) => pos }
      val inst = prog.getInst(pc)
      inst.op match
        case InstOp.MATCH =>
          '{
            (pos: Int) =>
              if pos == $input.length then pos else -1
          }

        case InstOp.FAIL =>
          '{ (_: Int) => -1 }

        case InstOp.ALT =>
          lazy val left = compile(inst.out, end, input)
          lazy val right = compile(inst.arg, end, input)
          '{
            (pos: Int) =>
              lazy val lp = $left(pos)
              if lp >= 0 then lp else $right(pos)
          }

        case InstOp.RUNE | InstOp.RUNE1 =>
          val runeCheck = inst.matchRuneExpr
          val outExpr = compile(inst.out, end, input)
          '{
            (pos: Int) =>
              if pos < $input.length && $runeCheck($input.charAt(pos).toInt)
                then $outExpr(pos + 1)
                else -1
          }
        
        case InstOp.LOOP =>
          // will need to be careful with lazy */+
          val loopStart = inst.out
          val loopEnd   = pc
          val bodyExpr  = compile(loopStart, loopEnd, input)
          val exitExpr  = compile(inst.arg, end, input)
          '{
            def loop(pos: Int): Int =
              val next = $bodyExpr(pos)
              if next >= 0 && next != pos then
                val attempt = loop(next) 
                if attempt >= 0 then attempt
                else $exitExpr(pos)
              else $exitExpr(pos)

            loop
          }

        case InstOp.CAPTURE =>
          val nextExpr = compile(inst.out, end, input)
          '{
            (pos: Int) =>
              val curPos = pos
              val res = $nextExpr(pos)
              if res >= 0 then res else -1
          }


        case _ =>
          quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")

    '{
      (input: CharSequence) =>
        val matcher = ${ compile(prog.start, prog.numInst, 'input) }
        matcher(0) >= 0
    }

  def genMatcher(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =
    def compile(
      pc: Int,
      end: Int,
      input: Expr[CharSequence],
      pos: Expr[Int],
    ): Expr[Int] = pc match
      case _ if pc == end =>
        pos

      case _ =>
        val inst = prog.getInst(pc)
        inst.op match

          case InstOp.MATCH =>
            '{
              if $pos == $input.length then
                $pos
              else
                -1
            }

          case InstOp.FAIL =>
            '{ -1 }

          case InstOp.ALT =>
            val left  = compile(inst.out, end, input, pos)
            val right = compile(inst.arg, end, input, pos)
            '{
              val lp = $left
              if lp >= 0 then lp
              else
                $right
            }

          case InstOp.RUNE | InstOp.RUNE1 =>
            val runeCheck = inst.matchRuneExpr
            val nextPos = '{ $pos + 1 }
            val succ = compile(inst.out, end, input, nextPos)
            '{
              if ($pos < $input.length && $runeCheck($input.charAt($pos).toInt))
                then 
                  $succ
                else -1
            }

          case InstOp.LOOP =>
            val body = (p: Expr[Int]) => compile(inst.out, pc,  input, p)
            val exit = (p: Expr[Int]) => compile(inst.arg, end, input, p)
            '{
              def loop(pos: Int): Int =
                val next   = ${ body('{pos}) }
                // if no match or zero-length, restore and exit
                if next == -1 || next == pos then
                  ${ exit('{pos}) }
                else
                  val attempt = loop(next)
                  if attempt >= pos then attempt
                  else
                    ${ exit('{pos}) }
              loop($pos)
            }

          case InstOp.CAPTURE =>
            val nextExp = compile(inst.out, end, input, pos)
            '{
              val res     = $nextExp
              if (res >= 0) then res else -1
            }

          case _ =>
            quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")

    '{
      (input: CharSequence) =>
        val result = ${ compile(prog.start, prog.numInst, 'input, '{0}) }
        result >= 0
    }
    

  def genMatcherWithCapsNaive(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =
    def compile(
      pc: Int,
      end: Int,
      input: Expr[CharSequence],
      pos: Expr[Int],
      cap: Expr[Array[Int]]
    ): Expr[Int] = pc match
      case _ if pc == end =>
        pos

      case _ =>
        val inst = prog.getInst(pc)
        inst.op match

          case InstOp.MATCH =>
            '{

              if $pos == $input.length then
                $cap(1) = $pos
                $pos
              else
                -1
            }

          case InstOp.FAIL =>
            '{ -1 }

          case InstOp.ALT =>
            val left  = compile(inst.out, end, input, pos, cap)
            val right = compile(inst.arg, end, input, pos, cap)
            '{
              // val oldCap = $cap.clone()
              val lp = $left
              if lp >= 0 then lp
              else
                // Array.copy(oldCap, 0, $cap, 0, $cap.length)
                $right
            }

          case InstOp.RUNE | InstOp.RUNE1 =>
            val runeCheck = inst.matchRuneExpr
            val nextPos   = '{ $pos + 1 }
            val succ      = compile(inst.out, end, input, nextPos, cap)
            '{
              if ($pos < $input.length && $runeCheck($input.charAt($pos).toInt))
                then $succ
                else -1
            }

          case InstOp.LOOP =>
            val body = (p: Expr[Int]) => compile(inst.out, pc,  input, p, cap)
            val exit = (p: Expr[Int]) => compile(inst.arg, end, input, p, cap)
            '{
              def loop(pos: Int): Int =
                println(s"cloning")
                val oldCap = $cap.clone()
                val next = ${ body('{pos}) }
                // if no match or zero-length, restore and exit
                if next == -1 || next == pos then
                  // println(s"restoring cap: ${$cap.mkString(", ")}, oldCap: ${oldCap.mkString(", ")}")
                  Array.copy(oldCap, 0, $cap, 0, $cap.length)
                  ${ exit('{pos}) }
                else
                  val attempt = loop(next)
                  if attempt >= pos then attempt
                  else
                    println(s"restoring cap: ${$cap.mkString(", ")}, oldCap: ${oldCap.mkString(", ")}")
                    Array.copy(oldCap, 0, $cap, 0, $cap.length)
                    ${ exit('{pos}) }

              loop($pos)
            }

          case InstOp.CAPTURE =>
            val slot    = inst.arg
            val nextExp = compile(inst.out, end, input, pos, cap)
            '{
              // val oldCap = $cap.clone()
              val oldVal = $cap(${Expr(slot)})
              val curPos  = $pos
              val res     = $nextExp

              if (res >= 0) then
                $cap(${Expr(slot)}) = curPos
                res
              else
                $cap(${Expr(slot)}) = oldVal
                -1
            }

          case _ =>
            quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")

    '{
      (input: CharSequence) =>
        val cap = new Array[Int](${Expr(prog.numCap)})
        val result = ${ compile(prog.start, prog.numInst, 'input, '{0}, 'cap) }
        if result == input.length then
          println(cap.mkString(" "))
          true
        else
          false
    }
    
  // def matches(prog: Prog, input: CharSequence): Boolean = {
  //   def compile(pc: Int, end: Int, input: CharSequence, pos: Int): Int = {
  //     if (pc == end) return pos

  //     val inst = prog.getInst(pc)

  //     inst.op match {
  //       case InstOp.MATCH =>
  //         if (pos == input.length) pos else -1

  //       case InstOp.FAIL =>
  //         -1

  //       case InstOp.ALT =>
  //         val left = compile(inst.out, end, input, pos)
  //         if (left >= 0) left else compile(inst.arg, end, input, pos)

  //       case InstOp.RUNE | InstOp.RUNE1 =>
  //         if (pos < input.length && inst.matchRune(input.charAt(pos).toInt))
  //           compile(inst.out, end, input, pos + 1)
  //         else -1

  //       case InstOp.LOOP =>
  //         def loop(pos0: Int): Int = {
  //           val next = compile(inst.out, pc, input, pos0)
  //           if (next >= 0 && next != pos0) {
  //             val attempt = loop(next)
  //             if (attempt >= 0) attempt
  //             else compile(inst.arg, end, input, pos0)
  //           } else {
  //             compile(inst.arg, end, input, pos0)
  //           }
  //         }
  //         loop(pos)

  //       case _ =>
  //         throw new RuntimeException(s"Unsupported op: ${inst.op}")
  //     }
  //   }

  //   compile(prog.start, prog.numInst, input, 0) >= 0
  // }

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

  def genMatcherWithCaps(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =
    def compile(
      pc: Int,
      end: Int,
      input: Expr[CharSequence],
      pos: Expr[Int],
      capCopy: Expr[Array[Int]],
      cap: Expr[Array[Int]]
    ): Expr[Int] = pc match
      case _ if pc == end =>
        pos

      case _ =>
        val inst = prog.getInst(pc)
        inst.op match

          case InstOp.MATCH =>
            '{

              if $pos == $input.length then
                $cap(1) = $pos
                $pos
              else
                -1
            }

          case InstOp.FAIL =>
            '{ -1 }

          case InstOp.ALT =>
            val left  = compile(inst.out, end, input, pos, capCopy, cap)
            val right = compile(inst.arg, end, input, pos, capCopy, cap)
            '{
              // val oldCap = $cap.clone()
              val lp = $left
              if lp >= 0 then lp
              else
                // Array.copy(oldCap, 0, $cap, 0, $cap.length)
                $right
            }

          case InstOp.RUNE | InstOp.RUNE1 =>
            val runeCheck = inst.matchRuneExpr
            val nextPos = '{ $pos + 1 }
            val succ = compile(inst.out, end, input, nextPos, capCopy, cap)
            '{
              if ($pos < $input.length && $runeCheck($input.charAt($pos).toInt))
                then $succ
                else -1
            }

          case InstOp.LOOP =>
            val body = (p: Expr[Int]) => compile(inst.out, pc,  input, p, capCopy, cap)
            val exit = (p: Expr[Int]) => compile(inst.arg, end, input, p, capCopy, cap)
            '{
              def loop(pos: Int): Int =
                // println(s"cloning")
                Array.copy($cap, 0, $capCopy, 0, $cap.length)
                val next = ${ body('{pos}) }
                // if no match or zero-length, restore and exit
                if next == -1 || next == pos then
                  // println(s"restoring cap: ${$cap.mkString(", ")}, oldCap: ${$capCopy.mkString(", ")}")
                  Array.copy($capCopy, 0, $cap, 0, $cap.length)
                  ${ exit('{pos}) }
                else
                  val attempt = loop(next)
                  if attempt >= pos then attempt
                  else
                    // println(s"restoring cap: ${$cap.mkString(", ")}, oldCap: ${$capCopy.mkString(", ")}")
                    Array.copy($capCopy, 0, $cap, 0, $cap.length)
                    ${ exit('{pos}) }

              loop($pos)
            }

          case InstOp.CAPTURE =>
            val slot    = inst.arg
            val nextExp = compile(inst.out, end, input, pos, capCopy, cap)
            '{
              // val oldCap = $cap.clone()
              val oldVal = $cap(${Expr(slot)})
              val curPos  = $pos
              val res     = $nextExp

              if (res >= 0) then
                $cap(${Expr(slot)}) = curPos
                res
              else
                $cap(${Expr(slot)}) = oldVal
                -1
            }

          case _ =>
            quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")

    '{
      (input: CharSequence) =>
        val capCopy = new Array[Int](${Expr(prog.numCap)})
        val groups = new Array[Int](${Expr(prog.numCap)})
        val result = ${ compile(prog.start, prog.numInst, 'input, '{0}, 'capCopy, 'groups) }

        // Debug group output
        println("groups: " + groups.mkString(", "))

        result >= 0
    }

@main def testProgMatcher(): Unit =
  val PatternResult(pat, numGroups, _, _) = Pattern.compile("(a*b*)*bc")
  val prog = ProgramCompiler.compileRegexp(pat, numGroups)
  println("prog: " + BacktrackingProgMatcher.matches(prog, "abababc"))