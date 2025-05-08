package oregano.internal

import scala.quoted.*
import scala.reflect.ClassTag
import scala.annotation.switch

type FunctionTable = Array[(Int, CharSequence, ThreadQueue, ThreadQueue) => Boolean]

// object VMCodegenLinear:
  // def epsilonClosure(prog: Prog, startPc: Int): List[Int] =
  //   val visited = scala.collection.mutable.Queue.empty[Int]
  //   val next = scala.collection.mutable.ListBuffer.empty[Int]

  //   visited.enqueue(startPc)
    
  //   while visited.nonEmpty do
  //     val pc = visited.dequeue()
  //     // if ALT/LOOP, queue both branches
  //     // else if NOP, queue the out branch
  //     // else, add to next
  //     prog.getInst(pc).op match
  //       case InstOp.NOP =>
  //         next += prog.getInst(pc).out
  //       case InstOp.ALT | InstOp.LOOP =>
  //         val inst = prog.getInst(pc)
  //         visited.enqueue(inst.out)
  //         visited.enqueue(inst.arg)
  //       case _ =>
  //         next += pc

  //   next.toList

  // def generateHandler(prog: Prog, pc: Int, inst: Inst)(using Quotes): Expr[(Int, CharSequence, ThreadQueue, ThreadQueue) => Boolean] =
  //   val out = Expr(inst.out)
  //   val outInst = Expr(prog.getInst(inst.out))
  //   lazy val runeCheck = inst.matchRuneExpr

  //   inst.op match
  //     case InstOp.MATCH =>
  //       '{
  //         (pos, input, _, _) =>
  //           pos == input.length
  //       }

  //     case InstOp.RUNE | InstOp.RUNE1 =>
  //       '{
  //         (pos, input, _, nextq) =>
  //           if pos < input.length && $runeCheck(input.charAt(pos).toInt) then
  //             if !nextq.contains($out) then
  //               val id = nextq.add($out)
  //               nextq.setThread(id, $outInst, pos + 1)
  //           false
  //       }

  //     case InstOp.NOP | InstOp.ALT | InstOp.LOOP =>
  //       val closure = epsilonClosure(prog, pc)

  //       '{
  //         (pos, _, runq, _) =>
  //           ${
  //             val enqueueExprs = closure.map { targetPc =>
  //               val pcExpr = Expr(targetPc)
  //               val instExpr = Expr(prog.getInst(targetPc))
  //               '{
  //                 if !runq.contains($pcExpr) then
  //                   val id = runq.add($pcExpr)
  //                   runq.setThread(id, $instExpr, pos)
  //               }
  //             }
  //             Expr.block(enqueueExprs, '{ false })
  //           }
  //       }

  //     case _ =>
  //       '{ (_, _, _, _) => false }

  // def buildTable(prog: Prog)(using Quotes): Expr[FunctionTable] =
  //   val handlerExprs: Seq[Expr[(Int, CharSequence, ThreadQueue, ThreadQueue) => Boolean]] =
  //     (0 until prog.numInst).toSeq.map { pc =>
  //       val inst = prog.getInst(pc)
  //       generateHandler(prog, pc, inst)
  //     }

  //   '{ Array(${Varargs(handlerExprs)}*) }

  // def genMatcherRE2(prog: Prog)(using Quotes): Expr[(CharSequence, FunctionTable) => Boolean] =
  //   '{
  //     (input: CharSequence, table: FunctionTable) =>
  //       var runq = ThreadQueue(${Expr(prog.numInst)})
  //       var nextq = ThreadQueue(${Expr(prog.numInst)})
  //       val startPc = ${Expr(prog.start)}
  //       val startInst = ${Expr(prog.getInst(prog.start))}
  //       val startId = runq.add(startPc)
  //       runq.setThread(startId, startInst, 0)

  //       var matched = false

  //       while !matched && !runq.isEmpty do
  //         var i = 0
  //         while i < runq.size do
  //           val pc = runq.densePcs(i)
  //           val pos = runq.getPos(i)
  //           matched ||= table(pc)(pos, input, runq, nextq)
  //           i += 1
  //         runq.clear()
  //         val tmp = runq
  //         runq = nextq
  //         nextq = tmp

  //       matched
  //     }


object MatcherFlat {
  def matches(input: CharSequence, table: FlatTable): Boolean = {
    val length = input.length
    var runq  = FlatQueue(table.n)
    var nextq = FlatQueue(table.n)
    var matched = false

    runq.add(table.start, 0)

    while (!matched && !runq.isEmpty) {
      var i = 0
      while (i < runq.size) {
        val pc  = runq.densePcs(i)
        val pos = runq.densePos(i)
        i += 1

        // table.op(pc) match {
        //   case x if x == InstOp.MATCH.ordinal =>
        //     if (pos == length) matched = true

        //   case x if x == InstOp.RUNE.ordinal || x == InstOp.RUNE1.ordinal =>
        //     if (pos < length && table.runes(pc)(input.charAt(pos).toInt))
        //       nextq.add(table.out(pc), pos + 1)

        //   case x if x == InstOp.NOP.ordinal =>
        //     runq.add(table.out(pc), pos)

        //   case x if x == InstOp.ALT.ordinal || x == InstOp.LOOP.ordinal =>
        //     runq.add(table.out(pc), pos)
        //     runq.add(table.arg(pc), pos)

        //   case x if x == InstOp.FAIL.ordinal =>
        //     () // do nothing
          
        //   case _ =>
        //     throw new RuntimeException(s"Unexpected opcode: ${table.op(pc)}")
        // }
        /* 
        as it stands, it seems likely that Prog will stay live, 
        may be worth examining the implications of matching on Prog, 
        this is naturally ugly (but performant)!
         */
        (table.op(pc): @switch) match
          case 5 => // MATCH
            if pos == length then matched = true

          case 7 | 8 => // RUNE, RUNE1
            if pos < length && table.runes(pc)(input.charAt(pos).toInt) then
              nextq.add(table.out(pc), pos + 1)

          case 6 => // NOP
            runq.add(table.out(pc), pos)

          case 0 | 11 => // ALT, LOOP
            runq.add(table.out(pc), pos)
            runq.add(table.arg(pc), pos)

          case _ => ()
      }

      runq.clear()
      val tmp = runq; runq = nextq; nextq = tmp
    }

    matched
  }
}

object MatcherFlatProg {
  def matches(prog: Prog, input: CharSequence): Boolean = {
    val inputLength = input.length
    var runq = FlatQueue(prog.numInst)
    var nextq = FlatQueue(prog.numInst)
    var matched = false

    def add(q: FlatQueue, pc: Int, pos: Int): Unit = {
      if (!q.contains(pc)) {
        val inst = prog.getInst(pc)
        inst.op match {
          case InstOp.FAIL => ()
          case InstOp.NOP  => add(q, inst.out, pos)
          case InstOp.ALT | InstOp.LOOP =>
            add(q, inst.out, pos)
            add(q, inst.arg, pos)
          case _ =>
            q.add(pc, pos)
            ()
        }
      }
    }

    def step(runq: FlatQueue, nextq: FlatQueue): Boolean = {
      var i = 0
      while (i < runq.size) {
        val pc = runq.densePcs(i)
        val pos = runq.densePos(i)
        val inst = prog.getInst(pc)

        inst.op match {
          case InstOp.MATCH =>
            if (pos == inputLength) return true

          case InstOp.RUNE | InstOp.RUNE1 =>
            if (pos < inputLength && inst.matchRune(input.charAt(pos).toInt))
              add(nextq, inst.out, pos + 1)

          case _ =>
            throw new RuntimeException(s"Unexpected opcode in step: ${inst.op}")
        }

        i += 1
      }

      false
    }

    add(runq, prog.start, 0)

    while (!matched && !runq.isEmpty) {
      matched = step(runq, nextq)
      runq.clear()
      val tmp = runq; runq = nextq; nextq = tmp
    }

    matched
  }
}