package oregano.internal

import scala.quoted.*

// may want something mutable, so can preallocate instead
// note that re2j stores an inst, one less layer of indirection
// case class Thread(pc: Int, pos: Int) 

final class Thread(var inst: Inst, var pos: Int) 

final class ThreadQueue(n: Int):
  val sparse = new Array[Int](n)
  val densePcs = new Array[Int](n)
  val denseInsts = new Array[Inst](n)
  val densePos = Array.fill(n)(-1)
  var size = 0

  def isEmpty: Boolean = size == 0

  def contains(pc: Int): Boolean =
    val j = sparse(pc)
    j < size && densePcs(j) == pc

  def add(pc: Int): Int =
    val j = size
    sparse(pc) = j
    densePcs(j) = pc
    size += 1
    j

  def clear(): Unit =
    size = 0

  def getInst(i: Int): Inst = denseInsts(i)
  def getPos(i: Int): Int = densePos(i)

  def setThread(i: Int, inst: Inst, pos: Int): Unit =
    denseInsts(i) = inst
    densePos(i) = pos

  override def toString: String =
    densePcs.take(size).mkString("{", ", ", "}")

// final case class Thread(inst: Inst, pos: Int) 

// final class ThreadQueue(n: Int):
//   val sparse = new Array[Int](n)
//   val densePcs = new Array[Int](n)
//   val denseThreads = new Array[Option[Thread]](n)
//   var size = 0

//   def isEmpty: Boolean = size == 0

//   def contains(pc: Int): Boolean =
//     val j = sparse(pc)
//     j < size && densePcs(j) == pc

//   def add(pc: Int): Int =
//     val j = size
//     sparse(pc) = j
//     densePcs(j) = pc
//     denseThreads(j) = None
//     size += 1
//     j

//   def clear(): Unit =
//     size = 0

//   def getThread(i: Int): Option[Thread] = denseThreads(i)
//   def setThread(i: Int, t: Thread): Unit = denseThreads(i) = Some(t)

//   override def toString: String =
//     densePcs.take(size).mkString("{", ", ", "}")

class LinearRuntimeMatcher(prog: Prog, input: CharSequence) {
  val inputLength = input.length()

  def add(q: ThreadQueue, pc: Int, pos: Int): Unit =
    // println(s"add($pc, $pos, ${prog.getInst(pc)})")
    if (!q.contains(pc))
      val inst = prog.getInst(pc)
      inst.op match
        case InstOp.FAIL => ()
        case InstOp.NOP  => add(q, inst.out, pos)
        case InstOp.ALT | InstOp.LOOP =>
          add(q, inst.out, pos)
          add(q, inst.arg, pos)
        case _ =>
          val id = q.add(pc)
          q.setThread(id, inst, pos) // optional: if storing Thread object

  def step(runq: ThreadQueue, nextq: ThreadQueue): Boolean =
    var i = 0
    while i < runq.size do
      val inst = runq.getInst(i)
      val pos = runq.getPos(i)
      inst.op match
        case InstOp.MATCH =>
          if pos == inputLength then return true

        case InstOp.RUNE | InstOp.RUNE1 =>
          if pos < inputLength && inst.matchRune(input.charAt(pos).toInt) then
            add(nextq, inst.out, pos + 1)

        case _ =>
          throw new RuntimeException(s"Unexpected opcode in step: ${inst.op}")
      i += 1
    false
}

object LinearRuntimeMatcher {
  def matches(prog: Prog, input: CharSequence): Boolean =
    val matcher = LinearRuntimeMatcher(prog, input)
    var runq = ThreadQueue(prog.numInst)
    var nextq = ThreadQueue(prog.numInst)

    matcher.add(runq, prog.start, 0)

    var matched = false
    while !matched && !runq.isEmpty do
      matched = matcher.step(runq, nextq)
      runq.clear()
      val tmp = runq
      runq = nextq
      nextq = tmp

    matched
}
type FunctionTable = Array[(Int, CharSequence, ThreadQueue, ThreadQueue) => Boolean]

object LinearMatcher:
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
        case InstOp.NOP | InstOp.CAPTURE =>
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

  def genMatcher(prog: Prog)(using Quotes): Expr[(CharSequence, FunctionTable, ThreadQueue, ThreadQueue) => Boolean] =
    '{
      (input: CharSequence, table: FunctionTable, q0: ThreadQueue, q1: ThreadQueue) =>
        var runq = q0
        var nextq = q1
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

        q0.clear(); q1.clear()
        matched
      }


@main def vmTest: Unit = {
  println("Basic alternation:")
  val PatternResult(basicPattern, basicPatternCaps, _) = Pattern.compile("a|b")
  val basicProg = ProgramCompiler.compileRegexp(basicPattern, basicPatternCaps)

  println(LinearRuntimeMatcher.matches(basicProg, "a")) // true
  println(LinearRuntimeMatcher.matches(basicProg, "b")) // true
  println(LinearRuntimeMatcher.matches(basicProg, "c")) // false
  println(LinearRuntimeMatcher.matches(basicProg, ""))  // false

  println("\nConcatenation:")
  val PatternResult(concatPattern, concatPatternCaps, _) = Pattern.compile("ab")
  val concatProg = ProgramCompiler.compileRegexp(concatPattern, concatPatternCaps)
  println(LinearRuntimeMatcher.matches(concatProg, "ab"))  // true
  println(LinearRuntimeMatcher.matches(concatProg, "a"))   // false
  println(LinearRuntimeMatcher.matches(concatProg, "abc")) // false

  println("\nLiteral + alternation:")
  val PatternResult(complex, complexCaps, _) = Pattern.compile("ab|cd")
  val complexProg = ProgramCompiler.compileRegexp(complex, complexCaps)
  println(LinearRuntimeMatcher.matches(complexProg, "ab")) // true
  println(LinearRuntimeMatcher.matches(complexProg, "cd")) // true
  println(LinearRuntimeMatcher.matches(complexProg, "ac")) // false
  println(LinearRuntimeMatcher.matches(complexProg, "abcd")) // false

  println("\nCharacter class:")
  val PatternResult(classPattern, classPatternCaps, _) = Pattern.compile("[a-z]")
  val classProg = ProgramCompiler.compileRegexp(classPattern, classPatternCaps)
  println(LinearRuntimeMatcher.matches(classProg, "a"))  // true
  println(LinearRuntimeMatcher.matches(classProg, "z"))  // true
  println(LinearRuntimeMatcher.matches(classProg, "A"))  // false
  println(LinearRuntimeMatcher.matches(classProg, "0"))  // false

  println("\nKleene Star:")
  val PatternResult(starPattern, starPatternCaps, _) = Pattern.compile("(a|b)*ab")
  val starProg = ProgramCompiler.compileRegexp(starPattern, starPatternCaps)
  println(LinearRuntimeMatcher.matches(starProg, "ab"))  // true
  println(LinearRuntimeMatcher.matches(starProg, "aab")) // true
  println(LinearRuntimeMatcher.matches(starProg, ""))   // false
  println(LinearRuntimeMatcher.matches(starProg, "b"))  // false

  println("\nNested Loops:")
  val PatternResult(nestPattern, nestPatternCaps, _) = Pattern.compile("((a)*b*)*")
  val nestProg = ProgramCompiler.compileRegexp(starPattern, starPatternCaps)
  println(LinearRuntimeMatcher.matches(nestProg, "ab"))  // true
  println(LinearRuntimeMatcher.matches(nestProg, "aab")) // true
  println(LinearRuntimeMatcher.matches(nestProg, "aaabaaa")) // true
  println(LinearRuntimeMatcher.matches(nestProg, "b"))  // true
}
