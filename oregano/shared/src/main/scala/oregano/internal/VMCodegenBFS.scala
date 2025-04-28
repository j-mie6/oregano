package oregano.internal

import scala.quoted.*
import scala.collection.mutable
import scala.reflect.ClassTag

object VMCodegenBFS:

  def genMatcher(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =

    def genHandler(inst: Inst): Expr[(Int, CharSequence, mutable.Queue[(Int, Int)], mutable.Set[(Int, Int)]) => Unit] =
      val out = Expr(inst.out)
      val arg = Expr(inst.arg)
      lazy val runeCheck = inst.matchRuneExpr

      inst.op match
        case InstOp.MATCH =>
          '{
            (pos, input, queue, visited) =>
              // very ugly, need to think about return type a bit
              if pos == input.length then throw new scala.util.control.ControlThrowable("MATCH") {}
          }

        case InstOp.RUNE | InstOp.RUNE1 =>
          '{
            (pos, input, queue, visited) =>
              if pos < input.length && $runeCheck(input.charAt(pos).toInt) then
                val next = ($out, pos + 1)
                if !visited.contains(next) then
                  visited.add(next)
                  queue.enqueue(next)
          }

        case InstOp.ALT | InstOp.LOOP =>
          '{
            (pos, input, queue, visited) =>
              val o = ($out, pos)
              val a = ($arg, pos)
              if !visited.contains(o) then
                visited.add(o)
                queue.enqueue(o)
              if !visited.contains(a) then
                visited.add(a)
                queue.enqueue(a)
          }

        case InstOp.NOP =>
          '{
            (pos, input, queue, visited) =>
              val next = ($out, pos)
              if !visited.contains(next) then
                visited.add(next)
                queue.enqueue(next)
          }

        case _ =>
          '{
            (_: Int, _: CharSequence, _: mutable.Queue[(Int, Int)], _: mutable.Set[(Int, Int)]) => ()
          }

    val handlers: Seq[Expr[(Int, CharSequence, mutable.Queue[(Int, Int)], mutable.Set[(Int, Int)]) => Unit]] =
      (0 until prog.numInst).toSeq.map(pc => genHandler(prog.getInst(pc)))
    
    val tableExpr: Expr[Seq[(Int, CharSequence, mutable.Queue[(Int, Int)], mutable.Set[(Int, Int)]) => Unit]] =
      Expr.ofSeq(handlers)

    '{
      (input: CharSequence) =>
        val queue = mutable.Queue.empty[(Int, Int)]
        val visited = mutable.Set.empty[(Int, Int)]
        val table = $tableExpr.toArray
        val tableSize = table.length

        queue.enqueue((${Expr(prog.start)}, 0))

        try
          while queue.nonEmpty do
            val (pc, pos) = queue.dequeue()
            if pc >= 0 && pc < tableSize then
              table(pc)(pos, input, queue, visited)
          false
        catch case _: scala.util.control.ControlThrowable => true
    }