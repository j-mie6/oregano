package oregano.internal

import scala.quoted.*

type StepFn = (RE2Machine, Queue, Queue, Int, Int) => Boolean
type AddFn = (RE2Machine, Int, Int, Array[Int], Queue, Thread) => Thread

object StagedMachine:
  def buildMachineTable(prog: Prog)(using Quotes): Expr[Array[StepFn]] =
    val handlers = (0 `until` prog.numInst).map { pc =>
      val inst = prog.getInst(pc)
      generateStep(prog, pc, inst)
    }
    '{Array(${ Varargs(handlers) }*)}

  def generateStep(prog: Prog, pc: Int, inst: Inst)(using Quotes): Expr[StepFn] =
    lazy val runeCheck = inst.matchRuneExpr
    lazy val addExpr = generateAdd(prog, inst.out, prog.getInst(inst.out))
    val outPcExpr = Expr(inst.out)

    inst.op match
      case InstOp.MATCH =>
        '{ (m, runq, _, pos, _) =>
            val t = runq.getThread(${Expr(pc)})
            if m.ncap > 0 && (!m.matched || m.matchcap(1) < pos) then
              t.cap(1) = pos
              System.arraycopy(t.cap, 0, m.matchcap, 0, m.ncap)
            m.matched = true
            true
          }

      case InstOp.RUNE | InstOp.RUNE1 | InstOp.RUNE_ANY | InstOp.RUNE_ANY_NOT_NL =>
        '{ (m, runq, nextq, pos, rune) =>
            val t = runq.getThread(${Expr(pc)})
            // println(runq.denseThreads.mkString(", "))
            // println(runq.sparse.mkString(", "))
            var matched = false

            matched = $runeCheck(rune) // staging w

            if matched then
              val _ = $addExpr(m, pos + 1, ${outPcExpr}, t.cap, nextq, t)
            if t != null then
              m.free(t)
              runq.clearThread(${Expr(pc)})
            false
          }

      case _ =>
        // do nothing
        '{ (_, _, _, _, _) => false }



  def generateAdd(prog: Prog, pc: Int, inst: Inst)(using Quotes): Expr[AddFn] =
    val pcExpr = Expr(pc)

    inst.op match
      case InstOp.FAIL =>
        '{ (_, _, _, _, _, t) => t }

      case InstOp.NOP =>
        generateAdd(prog, inst.out, prog.getInst(inst.out))

      case InstOp.ALT | InstOp.LOOP =>
        val left  = generateAdd(prog, inst.out, prog.getInst(inst.out))
        val right = generateAdd(prog, inst.arg, prog.getInst(inst.arg))
        '{ (m, pos, pc, cap, q, t) =>
            val t1 = $left(m, pos, pc, cap, q, t)
            val t2 = $right(m, pos, pc, cap, q, t1)
            t2
          }

      case InstOp.CAPTURE =>
        if inst.arg < prog.numCap then
          val slotExpr = Expr(inst.arg)
          val innerAdd = generateAdd(prog, inst.out, prog.getInst(inst.out))
          '{ (m, pos, pc, cap, q, t) =>
              val old = cap($slotExpr)
              cap($slotExpr) = pos
              val _ = $innerAdd(m, pos, pc, cap, q, null)
              cap($slotExpr) = old
              t
            }
        else
          generateAdd(prog, inst.out, prog.getInst(inst.out))

      case InstOp.MATCH | InstOp.RUNE | InstOp.RUNE1 | InstOp.RUNE_ANY | InstOp.RUNE_ANY_NOT_NL =>
        '{ (m, pos, _, cap, q, t) =>
            if q.contains(${pcExpr}) then t
            else
              val d = q.add(${pcExpr})
              val thread = if t == null then m.alloc(${Expr(inst)}) else { t.inst = ${Expr(inst)}; t }
              if (thread.cap eq cap) then
                thread.cap = cap.clone()
              else if m.ncap > 0 && (thread.cap ne cap) then
                System.arraycopy(cap, 0, thread.cap, 0, m.ncap)
              q.denseThreads(d) = thread
              null
          }

      case _ =>
        quotes.reflect.report.errorAndAbort(s"Unsupported op in generateAdd: ${inst.op}")

  def genMachineMatcher(prog: Prog)(using Quotes): Expr[(CharSequence, Array[StepFn], RE2Machine) => Boolean] =
    val startPc = Expr(prog.start)
    '{
      (input: CharSequence, table: Array[StepFn], m: RE2Machine) =>
        var runq = m.q0
        var nextq = m.q1
        m.matched = false
        m.matchcap.indices.foreach(i => m.matchcap(i) = 0)
        val addFn = ${generateAdd(prog, prog.start, prog.getInst(prog.start))}
        val _ = addFn(m, 0, $startPc, m.matchcap.clone(), runq, null)

        var pos = 0
        m.matched = false

        while !m.matched && !runq.isEmpty do
          var i = 0
          while i < runq.size do
            val pc = runq.densePcs(i)
            m.matched ||= table(pc)(m, runq, nextq, pos, if pos < input.length then input.charAt(pos).toInt else -1)
            i += 1
          runq.clear()
          val tmp = runq; runq = nextq; nextq = tmp
          if pos < input.length then pos += 1

        println(m.matchcap.mkString(" "))
        m.matched
    }