package oregano.internal

import scala.quoted.*

object VMCodegen:

  def genMatcher(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =
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

        case _ =>
          quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")

    '{
      (input: CharSequence) =>
        val matcher = ${ compile(prog.start, prog.numInst, 'input) }
        matcher(0) >= 0
    }
