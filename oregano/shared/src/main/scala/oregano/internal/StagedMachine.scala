package oregano.internal

import scala.quoted.*

type StepFn = (RE2Machine, RE2Queue, RE2Queue, Int, Int) => Boolean
type AddFn = (RE2Machine, Int, Int, Array[Int], RE2Queue, RE2Thread) => RE2Thread

object StagedMachine:
  def buildMachineTable(prog: Prog)(using Quotes): Expr[Array[StepFn]] =
    val handlers = (0 `until` prog.numInst).map { pc =>
      val inst = prog.getInst(pc)
      generateStep(prog, pc, inst)
    }
    '{Array(${ Varargs(handlers) }*)}

  def buildEpsilonTable(prog: Prog)(using Quotes): Expr[Array[AddFn]] =
    val handlers = (0 `until` prog.numInst).map { pc =>
      val inst = prog.getInst(pc)
      generateAdd(prog, pc, inst)
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
              // if (thread.cap eq cap) then
              //   thread.cap = cap.clone()
              if m.ncap > 0 && (thread.cap ne cap) then
                System.arraycopy(cap, 0, thread.cap, 0, m.ncap)
              q.denseThreads(d) = thread
              null
          }

      case _ =>
        quotes.reflect.report.errorAndAbort(s"Unsupported op in generateAdd: ${inst.op}")

  def generateInlineAdd(
    pc: Int,
    inst: Inst,
    prog: Prog,
    m: Expr[RE2Machine],
    cap: Expr[Array[Int]],
    pos: Expr[Int],
    q: Expr[RE2Queue],
    t: Expr[RE2Thread]
  )(using Quotes): Expr[RE2Thread] =
    val pcExpr = Expr(pc)

    inst.op match
      case InstOp.FAIL =>
        t

      case InstOp.NOP =>
        generateInlineAdd(inst.out, prog.getInst(inst.out), prog, m, cap, pos, q, t)

      case InstOp.ALT | InstOp.LOOP =>
        val addLeft  = generateInlineAdd(inst.out, prog.getInst(inst.out), prog, m, cap, pos, q, t)
        val addRight = generateInlineAdd(inst.arg, prog.getInst(inst.arg), prog, m, cap, pos, q, addLeft)
        addRight

      case InstOp.CAPTURE =>
        if inst.arg < prog.numCap then
          val slotExpr = Expr(inst.arg)
          val innerAdd = generateInlineAdd(inst.out, prog.getInst(inst.out), prog, m, cap, pos, q, '{ null })
          '{
            val old = $cap($slotExpr)
            $cap($slotExpr) = $pos
            val _ = $innerAdd
            $cap($slotExpr) = old
            $t
          }
        else
          generateInlineAdd(inst.out, prog.getInst(inst.out), prog, m, cap, pos, q, t)

      case InstOp.MATCH | InstOp.RUNE | InstOp.RUNE1 | InstOp.RUNE_ANY | InstOp.RUNE_ANY_NOT_NL =>
        // '{
        //   if $q.contains($pcExpr) then $t
        //   else
        //     val d = $q.add($pcExpr)
        //     val thread = if $t == null then $m.alloc(${ Expr(inst) }) else { $t.inst = ${ Expr(inst) }; $t }
        //     if $m.ncap > 0 && (thread.cap ne $cap) then
        //       System.arraycopy($cap, 0, thread.cap, 0, $m.ncap)
        //     $q.denseThreads(d) = thread
        //     null
        // }
        '{
          if $q.contains($pcExpr) then $t
          else
            val d = $q.add($pcExpr)
            val thread =
              if $t == null then $m.alloc(${ Expr(inst) })
              else
                val reused = $t // required because cannot reassign a param, compiler conservatively assumes val
                reused.inst = ${ Expr(inst) }
                reused

            if $m.ncap > 0 && (thread.cap ne $cap) then
              System.arraycopy($cap, 0, thread.cap, 0, $m.ncap)
            $q.denseThreads(d) = thread
            null
        }
      case _ =>
        quotes.reflect.report.errorAndAbort(s"Unsupported op in inline add: ${inst.op}")


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

        // println(m.matchcap.mkString(" "))
        m.matched
    }

  // def generateStepLoop(prog: Prog)(using Quotes): Expr[(RE2Machine, CharSequence) => Boolean] =
  //   import quotes.reflect.*

  //   '{
  //     (m: RE2Machine, input: CharSequence) =>
  //       var runq = m.q0
  //       var nextq = m.q1
  //       var pos = 0
  //       m.matched = false
  //       java.util.Arrays.fill(m.matchcap, 0)

  //       val addFn = ${ generateAdd(prog, prog.start, prog.getInst(prog.start)) }
  //       val _ = addFn(m, 0, ${ Expr(prog.start) }, m.matchcap.clone(), runq, null)

  //       while !m.matched && !runq.isEmpty do
  //         var i = 0
  //         while i < runq.size do
  //           val pc = runq.densePcs(i)
  //           val rune = if pos < input.length then input.charAt(pos).toInt else -1

  //           ${
  //             val cases: List[CaseDef] = (0 until prog.numInst).map { pc =>
  //               val inst = prog.getInst(pc)
  //               val body = generateStagedStepForPc(
  //                 pc,
  //                 inst,
  //                 prog,
  //                 '{ m },
  //                 '{ runq },
  //                 '{ nextq },
  //                 '{ pos },
  //                 '{ rune }
  //               )(using quotes)
  //               CaseDef(Literal(IntConstant(pc)), None, body.asTerm)
  //             }.toList

  //             Match('{ pc }.asTerm, cases).asExprOf[Unit]
  //           }

  //           i += 1
  //         runq.clear()
  //         val tmp = runq; runq = nextq; nextq = tmp
  //         if pos < input.length then pos += 1
        
  //       println(m.matchcap.mkString(" "))
  //       m.matched
  //   }

  def generateStepLoop(prog: Prog)(using Quotes): Expr[(RE2Machine, CharSequence) => Boolean] =
    import quotes.reflect.*

    def caseDefs(
      m: Expr[RE2Machine],
      runq: Expr[RE2Queue],
      nextq: Expr[RE2Queue],
      pos: Expr[Int],
      rune: Expr[Int]
    )(using Quotes): List[CaseDef] =
      (0 until prog.numInst).toList.map { pc =>
        val inst = prog.getInst(pc)
        val body = generateStagedStepForPc(
          pc,
          inst,
          prog,
          m,
          runq,
          nextq,
          pos,
          rune
        )
        CaseDef(Literal(IntConstant(pc)), None, body.asTerm)
      }

    '{
      (m: RE2Machine, input: CharSequence) =>
        var runq = m.q0
        var nextq = m.q1
        var pos = 0
        m.matched = false
        java.util.Arrays.fill(m.matchcap, 0)

        val _ = ${
          generateInlineAdd(
            prog.start,
            prog.getInst(prog.start),
            prog,
            '{ m },
            '{ m.matchcap.clone() },
            '{ 0 },
            '{ runq },
            '{ null }
          )
        }

        while !m.matched && !runq.isEmpty do
          var i = 0
          while i < runq.size do
            val pc = runq.densePcs(i)
            val rune = if pos < input.length then input.charAt(pos).toInt else -1

            ${
              val matchExpr = Match(
                '{ pc }.asTerm,
                caseDefs('{ m }, '{ runq }, '{ nextq }, '{ pos }, '{ rune })
              ).asExprOf[Unit]
              matchExpr
            }

            i += 1

          runq.clear()
          val tmp = runq; runq = nextq; nextq = tmp
          if pos < input.length then pos += 1

        // println(m.matchcap.mkString(" "))
        m.matched
    }


  def generateStagedStepForPc(
    pc: Int,
    inst: Inst,
    prog: Prog,
    m: Expr[RE2Machine],
    runq: Expr[RE2Queue],
    nextq: Expr[RE2Queue],
    pos: Expr[Int],
    rune: Expr[Int]
  )(using Quotes): Expr[Unit] =


    val pcExpr = Expr(pc)

    inst.op match
      case InstOp.MATCH =>
        '{
          val t = $runq.getThread($pcExpr)
          if $m.ncap > 0 && (!$m.matched || $m.matchcap(1) < $pos) then
            t.cap(1) = $pos
            System.arraycopy(t.cap, 0, $m.matchcap, 0, $m.ncap)
          $m.matched = true
        }

      case InstOp.RUNE | InstOp.RUNE1 | InstOp.RUNE_ANY | InstOp.RUNE_ANY_NOT_NL =>
        val runeCheck = inst.matchRuneExpr
        val add = generateAdd(prog, inst.out, prog.getInst(inst.out))
        val outPcExpr = Expr(inst.out)

        // '{
        //   var t = $runq.getThread($pcExpr)
        //   val matched = ${ runeCheck }($rune)
        //   if matched then
        //     // t = $add($m, $pos + 1, $outPcExpr, t.cap, $nextq, t)
        //     // t = $m.add($outPcExpr, $pos + 1, t.cap, $nextq, t)
        //     t = ${
        //       generateInlineAdd(
        //         inst.out,
        //         prog.getInst(inst.out),
        //         prog,
        //         m,
        //         '{ t.cap },
        //         '{ $pos + 1 },
        //         nextq,
        //         't
        //         )
        //       }
        //     if t != null then
        //       $m.free(t)
        //       $runq.clearThread($pcExpr)
        //   }
          '{
            val t = $runq.getThread($pcExpr)
            val matched = ${ runeCheck }($rune)
            if matched then
              val t2 = ${
                generateInlineAdd(
                  inst.out,
                  prog.getInst(inst.out),
                  prog,
                  m,
                  '{ t.cap },
                  '{ $pos + 1 },
                  nextq,
                  '{ t }
                )
              }
              if t2 != null then
                $m.free(t2)
                $runq.clearThread($pcExpr)
          }


      case _ =>
        '{ () } // No-op
