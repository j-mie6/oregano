package oregano.internal

import scala.quoted.*
import scala.collection.mutable
import scala.reflect.ClassTag

// def arrayExpr[T: Type](elems: Array[Expr[T]])(using Quotes): Expr[Array[T]] =
//   import quotes.reflect.*
// 
//   println(s"Creating array of ${elems.mkString(", ")}")
//   val arraySym = Symbol.requiredModule("scala.Array")
//   println(s"Array symbol")
//   val arrayTpe = TypeRepr.of[T]
//   println(s"Array type")
//   val arrayApply = Select.overloaded(
//     qualifier = Ref(arraySym),
//     name = "apply",
//     targs = List(arrayTpe),
//     args = List(Varargs(elems.toIndexedSeq).asTerm)
//   )
//   println(s"Array apply")
//   val res = arrayApply.asExprOf[Array[T]]
//   println("done")
//   res

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

  def generateHandler(prog: Prog, pc: Int, inst: Inst)(using Quotes): Expr[(Int, CharSequence, ThreadQueue, ThreadQueue) => Boolean] =
    lazy val out = Expr(inst.out)
    lazy val outInst = Expr(prog.getInst(inst.out))
    lazy val runeCheck = inst.matchRuneExpr
    lazy val closure = epsilonClosure(prog, pc)

    def staticEnqueues(posExpr: Expr[Int], runqExpr: Expr[ThreadQueue]): List[Expr[Unit]] =
      closure.map { target =>
        val targetExpr = Expr(target)
        val instExpr = Expr(prog.getInst(target))
        '{
          if !$runqExpr.contains($targetExpr) then
            val id = $runqExpr.add($targetExpr)
            $runqExpr.setThread(id, Thread($instExpr, $posExpr))
        }
      }


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
                nextq.setThread(id, Thread($outInst, pos + 1))
            false
        }

      // old, no epsilon closure:
      // case InstOp.NOP =>
      //   '{
      //     (pos, _input, runq, _nextq) =>
      //       if !runq.contains($out) then
      //         val id = runq.add($out)
      //         runq.setThread(id, Thread($outInst, pos))
      //       false
      //   }

      // case InstOp.ALT | InstOp.LOOP =>
      //   '{
      //     (pos, _input, runq, _nextq) =>
      //       if !runq.contains($out) then
      //         val id = runq.add($out)
      //         runq.setThread(id, Thread($outInst, pos))
      //       if !runq.contains($arg) then
      //         val id = runq.add($arg)
      //         runq.setThread(id, Thread($argInst, pos))
      //       false
      //   }
      
      case InstOp.NOP | InstOp.ALT | InstOp.LOOP =>
        '{
          (pos, _, runq, _) =>
            ${
              val stagedBlocks = staticEnqueues('{ pos }, '{ runq })
              Expr.block(stagedBlocks, '{ false })
            }
        }

      case _ =>
        '{ (_, _, _, _) => false }

  def genMatcherRE2(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =
    val handlerExprs: Seq[Expr[(Int, CharSequence, ThreadQueue, ThreadQueue) => Boolean]] =
      (0 until prog.numInst).toSeq.map { pc =>
        val inst = prog.getInst(pc)
        generateHandler(prog, pc, inst)
      }

    val tableExpr = Varargs(handlerExprs)

    '{
      (input: CharSequence) =>
        var runq = ThreadQueue(${Expr(prog.numInst)})
        var nextq = ThreadQueue(${Expr(prog.numInst)})
        // this can and should be extracted out and done once per expression
        val table = Array($tableExpr*)
        val startPc = ${Expr(prog.start)}
        val startId = runq.add(startPc)
        runq.setThread(startId, Thread(${Expr(prog.getInst(prog.start))}, 0))
        var matched = false


        while !matched && !runq.isEmpty do
          var i = 0
          // do the 'step' part
          while i < runq.size do
            val pc = runq.densePcs(i)
            val pos = runq.getThread(i).get.pos
            // should probably short circuit here
            matched ||= table(pc)(pos, input, runq, nextq)
            i += 1

          runq.clear()
          val tmp = runq
          runq = nextq
          nextq = tmp

        matched
    }


@main def closureShenanigans: Unit = {
  val basicPattern = Pattern.compile("a|b|c")
  val basicProg = ProgramCompiler.compileRegexp(basicPattern)
  println(basicProg)
  println(VMCodegenLinear.epsilonClosure(basicProg, basicProg.start))
}