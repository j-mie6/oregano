package oregano.internal

import scala.quoted.*
import scala.reflect.ClassTag

type FunctionTable = Array[(Int, CharSequence, ThreadQueue, ThreadQueue) => Boolean]

object VMCodegenLinear:
  def epsilonClosure(prog: Prog, startPc: Int): List[Int] =
    val visited = scala.collection.mutable.Queue.empty[Int]
    val next = scala.collection.mutable.ListBuffer.empty[Int]

    visited.enqueue(startPc)
    
    while visited.nonEmpty do
      val pc = visited.dequeue()
      // if ALT/LOOP, queue both branches
      // else if NOP, queue the out branch
      // else, add to next
      prog.getInst(pc).op match
        case InstOp.NOP =>
          next += prog.getInst(pc).out
        case InstOp.ALT | InstOp.LOOP =>
          val inst = prog.getInst(pc)
          visited.enqueue(inst.out)
          visited.enqueue(inst.arg)
        case _ =>
          next += pc

    next.toList

  def generateHandler(prog: Prog, pc: Int, inst: Inst)(using Quotes): Expr[(Int, CharSequence, ThreadQueue, ThreadQueue) => Boolean] =
    val out = Expr(inst.out)
    val outInst = Expr(prog.getInst(inst.out))
    lazy val runeCheck = inst.matchRuneExpr

    inst.op match
      case InstOp.MATCH =>
        '{
          (pos, input, _, _) =>
            pos == input.length
        }

      case InstOp.RUNE | InstOp.RUNE1 =>
        '{
          (pos, input, _, nextq) =>
            if pos < input.length && $runeCheck(input.charAt(pos).toInt) then
              if !nextq.contains($out) then
                val id = nextq.add($out)
                nextq.setThread(id, $outInst, pos + 1)
            false
        }

      case InstOp.NOP | InstOp.ALT | InstOp.LOOP =>
        val closure = epsilonClosure(prog, pc)

        '{
          (pos, _, runq, _) =>
            ${
              val enqueueExprs = closure.map { targetPc =>
                val pcExpr = Expr(targetPc)
                val instExpr = Expr(prog.getInst(targetPc))
                '{
                  if !runq.contains($pcExpr) then
                    val id = runq.add($pcExpr)
                    runq.setThread(id, $instExpr, pos)
                }
              }
              Expr.block(enqueueExprs, '{ false })
            }
        }

      case _ =>
        '{ (_, _, _, _) => false }

  def buildTable(prog: Prog)(using Quotes): Expr[FunctionTable] =
    val handlerExprs: Seq[Expr[(Int, CharSequence, ThreadQueue, ThreadQueue) => Boolean]] =
      (0 until prog.numInst).toSeq.map { pc =>
        val inst = prog.getInst(pc)
        generateHandler(prog, pc, inst)
      }

    '{ Array(${Varargs(handlerExprs)}*) }

  def genMatcherRE2(prog: Prog)(using Quotes): Expr[(CharSequence, FunctionTable) => Boolean] =
    '{
      (input: CharSequence, table: FunctionTable) =>
        var runq = ThreadQueue(${Expr(prog.numInst)})
        var nextq = ThreadQueue(${Expr(prog.numInst)})
        val startPc = ${Expr(prog.start)}
        val startInst = ${Expr(prog.getInst(prog.start))}
        val startId = runq.add(startPc)
        runq.setThread(startId, startInst, 0)

        var matched = false

        while !matched && !runq.isEmpty do
          var i = 0
          while i < runq.size do
            val pc = runq.densePcs(i)
            val pos = runq.getPos(i)
            matched ||= table(pc)(pos, input, runq, nextq)
            i += 1
          runq.clear()
          val tmp = runq
          runq = nextq
          nextq = tmp

        matched
      }