package oregano.internal

import scala.collection.mutable.Set
import scala.quoted.*

object StagedMachine:
  def generateInlineAdd(
      pc: Int,
      inst: Inst,
      prog: Prog,
      m: Expr[RE2Machine],
      cap: Expr[Array[Int]],
      pos: Expr[Int],
      q: Expr[RE2Queue],
      t: Expr[RE2Thread],
      seen: Set[Int] = Set[Int]()
  )(using Quotes): Expr[RE2Thread] =
    if seen.contains(pc) then
      // println(s"Already seen $pc")
      return t
    // println(s"Generating inline add for $pc")
    seen.add(pc)

    val pcExpr = Expr(pc)

    inst.op match
      case InstOp.FAIL =>
        t

      case InstOp.NOP =>
        generateInlineAdd(
          inst.out,
          prog.getInst(inst.out),
          prog,
          m,
          cap,
          pos,
          q,
          t,
          seen
        )

      case InstOp.ALT | InstOp.LOOP =>
        val addLeft = generateInlineAdd(
          inst.out,
          prog.getInst(inst.out),
          prog,
          m,
          cap,
          pos,
          q,
          t,
          seen
        )
        val addRight = generateInlineAdd(
          inst.arg,
          prog.getInst(inst.arg),
          prog,
          m,
          cap,
          pos,
          q,
          addLeft,
          seen
        )
        addRight

      case InstOp.CAPTURE =>
        if inst.arg < prog.numCap then
          val slotExpr = Expr(inst.arg)
          val innerAdd = generateInlineAdd(
            inst.out,
            prog.getInst(inst.out),
            prog,
            m,
            cap,
            pos,
            q,
            '{ null },
            seen
          )
          '{
            val old = $cap($slotExpr)
            $cap($slotExpr) = $pos
            val _ = $innerAdd
            $cap($slotExpr) = old
            $t
          }
        else
          generateInlineAdd(
            inst.out,
            prog.getInst(inst.out),
            prog,
            m,
            cap,
            pos,
            q,
            t,
            seen
          )

      case InstOp.MATCH | InstOp.RUNE | InstOp.RUNE1 | InstOp.RUNE_ANY |
          InstOp.RUNE_ANY_NOT_NL =>
        '{
          if $q.contains($pcExpr) then $t
          else
            val d = $q.add($pcExpr)
            val thread =
              if $t == null then $m.alloc($m.prog.insts($pcExpr))
              else
                val reused =
                  $t // required because cannot reassign a param, compiler conservatively assumes val
                reused.inst = $m.prog.insts($pcExpr)
                reused

            if $m.ncap > 0 && (thread.cap ne $cap) then
              System.arraycopy($cap, 0, thread.cap, 0, $m.ncap)
            $q.denseThreads(d) = thread
            null
        }
      case _ =>
        quotes.reflect.report.errorAndAbort(
          s"Unsupported op in inline add: ${inst.op}"
        )

  def generateStepLoop(prog: Prog)(using
      Quotes
  ): Expr[(RE2Machine, CharSequence) => Boolean] =
    import quotes.reflect.*

    def caseDefs(
        m: Expr[RE2Machine],
        runq: Expr[RE2Queue],
        nextq: Expr[RE2Queue],
        pos: Expr[Int],
        rune: Expr[Int]
        // addDispatcher: Expr[(Int, Array[Int], Int, RE2Queue, RE2Thread) => RE2Thread]
    )(using Quotes): List[CaseDef] =
      // val requiredInstOps = Set(
      //   InstOp.MATCH,
      //   InstOp.RUNE,
      //   InstOp.RUNE1,
      //   InstOp.RUNE_ANY,
      //   InstOp.RUNE_ANY_NOT_NL
      // )
      // (0 until prog.numInst).toList filter(
      //   pc => requiredInstOps.contains(prog.getInst(pc).op)
      // ) map { pc =>
      (0 until prog.numInst).toList map { pc =>
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

    '{ (m: RE2Machine, input: CharSequence) =>
      var runq = m.q0
      var nextq = m.q1
      var pos = 0
      m.matched = false
      m.matchcap.indices.foreach(i => m.matchcap(i) = 0)

      val clone = m.matchcap.clone()

      val _ = ${
        generateInlineAdd(
          prog.start,
          prog.getInst(prog.start),
          prog,
          '{ m },
          '{ clone },
          '{ 0 },
          '{ runq },
          '{ null }
        )
      }

      // val _ = m.add(
      //   $progStartExpr,
      //   0,
      //   m.matchcap.clone(),
      //   runq,
      //   null
      // )

      while !runq.isEmpty do
        var i = 0
        while i < runq.size do
          val pc = runq.densePcs(i)
          val rune = if pos < input.length then input.charAt(pos).toInt else -1

          // pretty cool: we basically say 'trust me, pc exists' then build a match expr!
          // should probably do nothing rather than returning cases when cases do nothing
          ${
            val matchExpr = Match(
              '{ pc }.asTerm,
              caseDefs('{ m }, '{ runq }, '{ nextq }, '{ pos }, '{ rune })
            ).asExprOf[Unit]
            // println("Generating match cases:")
            // caseDefs('{ m }, '{ runq }, '{ nextq }, '{ pos }, '{ rune }).foreach(cd => println(cd.pattern.show))
            matchExpr
          }

          i += 1

        runq.clear()
        val tmp = runq; runq = nextq; nextq = tmp
        if pos < input.length then pos += 1

      // println(m.matchcap.mkString(" "))
      // println(s"poolsize: ${m.poolSize}")
      m.matched
    }

  def generateStepHandlers(prog: Prog)(using
      Quotes
  ): Expr[Array[(Int, Int, RE2Queue, RE2Queue, Int, RE2Machine) => Unit]] =
    val handlerExprs
        : Seq[Expr[(Int, Int, RE2Queue, RE2Queue, Int, RE2Machine) => Unit]] =
      (0 until prog.numInst).toList.map { pc =>
        val inst = prog.getInst(pc)
        '{
          (
              rune: Int,
              pos: Int,
              runq: RE2Queue,
              nextq: RE2Queue,
              i: Int,
              m: RE2Machine
          ) =>
            ${
              generateStagedStepForPc(
                pc = pc,
                inst = inst,
                prog = prog,
                m = 'm,
                runq = 'runq,
                nextq = 'nextq,
                pos = 'pos,
                rune = 'rune
              )
            }
        }
      }

    '{ Array(${ Varargs(handlerExprs) }*) }

  def generateStagedStepForPc(
      pc: Int,
      inst: Inst,
      prog: Prog,
      m: Expr[RE2Machine],
      runq: Expr[RE2Queue],
      nextq: Expr[RE2Queue],
      pos: Expr[Int],
      rune: Expr[Int]
      // addDispatcher: Expr[(Int, Array[Int], Int, RE2Queue, RE2Thread) => RE2Thread]
  )(using Quotes): Expr[Unit] =

    val pcExpr = Expr(pc)

    inst.op match
      case InstOp.MATCH =>
        '{
          val t = $runq.getThread($pcExpr)
          if $rune == -1 && $m.anchorEnd || ! $m.anchorEnd
          then // should be some EOF, need to wrap inputs
            t.cap(1) = $pos
            Array.copy(t.cap, 0, $m.matchcap, 0, $m.ncap)
            $m.matched = true
          if t != null then
            $m.free(t)
            $runq.clearThread($pcExpr)
        }

      case InstOp.RUNE | InstOp.RUNE1 | InstOp.RUNE_ANY |
          InstOp.RUNE_ANY_NOT_NL =>
        val runeCheck = inst.matchRuneExpr
        '{
          var t = $runq.getThread($pcExpr)
          if t != null then
            // val matched = ${ runeCheck }($rune)
            val matched = ${
              Expr.betaReduce('{ $runeCheck($rune) })
            } // interestingly this is inlined in the 'inline everything' approach, but not ordinarily!
            if matched then
              // uncomment for recursive add
              // t = $m.add($instOutExpr, $pos + 1, t.cap, $nextq, t)
              t = ${
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
          if t != null then
            $m.free(t)
            $runq.clearThread($pcExpr)
        }

      case _ =>
        '{ () } // No-op
