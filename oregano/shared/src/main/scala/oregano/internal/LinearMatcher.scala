package oregano.internal

import scala.quoted.*

// may want something mutable, so can preallocate instead
// note that re2j stores an inst, one less layer of indirection
// case class Thread(pc: Int, pos: Int) 

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

class LinearRuntimeMatcher(prog: Prog) {
  val q1 = ThreadQueue(prog.numInst)
  val q2 = ThreadQueue(prog.numInst)  

  def add(q: ThreadQueue, pc: Int, pos: Int): Unit =
    if (!q.contains(pc))
      val inst = prog.getInst(pc) 
      val id = q.add(pc)
      q.setThread(id, inst, pos)
      inst.op match
        case InstOp.FAIL => ()
        case InstOp.NOP | InstOp.CAPTURE => 
          add(q, inst.out, pos)
        case InstOp.ALT | InstOp.LOOP =>
          add(q, inst.out, pos)
          add(q, inst.arg, pos)
        case _ => ()


  def step(pos: Int, rune: Int, runq: ThreadQueue, nextq: ThreadQueue): Boolean =
    var i = 0
    while i < runq.size do
      val inst = runq.getInst(i)
      val pos = runq.getPos(i)
      inst.op match
        case InstOp.MATCH =>
          if rune == -1 then return true

        case InstOp.RUNE | InstOp.RUNE1 =>
          if pos != -1 && inst.matchRune(rune) then
            add(nextq, inst.out, pos + 1)
        case _ => ()
      i += 1
    runq.clear()
    false

  def matches(input: CharSequence): Boolean =
    val inputLen = input.length
    var pos = 0
    var matched = false
    var runq = q1
    var nextq = q2

    add(runq, prog.start, 0)

    while !matched && !runq.isEmpty do
      val rune = if pos < inputLen then input.charAt(pos).toInt else -1 // EOF
      matched = step(pos, rune, runq, nextq)
      runq.clear()
      val tmp = runq
      runq = nextq
      nextq = tmp
      if pos < inputLen then
        pos += 1
      else
        pos = -1 // EOF

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
  val PatternResult(basicPattern, basicPatternCaps, _, _) = Pattern.compile("a|b")
  val basicProg = ProgramCompiler.compileRegexp(basicPattern, basicPatternCaps)
  val basicMatcher = new LinearRuntimeMatcher(basicProg)
  println(basicMatcher.matches("a")) // true
  println(basicMatcher.matches("b")) // true
  println(basicMatcher.matches("c")) // false
  println(basicMatcher.matches(""))  // false

  println("\nConcatenation:")
  val PatternResult(concatPattern, concatPatternCaps, _, _) = Pattern.compile("ab")
  val concatProg = ProgramCompiler.compileRegexp(concatPattern, concatPatternCaps)
  val concatMatcher = new LinearRuntimeMatcher(concatProg)
  println(concatMatcher.matches("ab"))  // true
  println(concatMatcher.matches("a"))   // false
  println(concatMatcher.matches("abc")) // false

  println("\nLiteral + alternation:")
  val PatternResult(complex, complexCaps, _, _) = Pattern.compile("ab|cd")
  val complexProg = ProgramCompiler.compileRegexp(complex, complexCaps)
  val complexMatcher = new LinearRuntimeMatcher(complexProg)
  println(complexMatcher.matches("ab")) // true
  println(complexMatcher.matches("cd")) // true
  println(complexMatcher.matches("ac")) // false
  println(complexMatcher.matches("abcd")) // false

  println("\nCharacter class:")
  val PatternResult(classPattern, classPatternCaps, _, _) = Pattern.compile("[a-z]")
  val classProg = ProgramCompiler.compileRegexp(classPattern, classPatternCaps)
  val classMatcher = new LinearRuntimeMatcher(classProg)
  println(classMatcher.matches("a"))  // true
  println(classMatcher.matches("z"))  // true
  println(classMatcher.matches("A"))  // false
  println(classMatcher.matches("0"))  // false

  println("\nKleene Star:")
  val PatternResult(starPattern, starPatternCaps, _, _) = Pattern.compile("(a|b)*ab")
  val starProg = ProgramCompiler.compileRegexp(starPattern, starPatternCaps)
  val starMatcher = new LinearRuntimeMatcher(starProg)
  println(starMatcher.matches("ab"))  // true
  println(starMatcher.matches("aab")) // true
  println(starMatcher.matches("aabaaab"))   // true
  println(starMatcher.matches(""))   // false
  println(starMatcher.matches("b"))  // false

  println("\nNested Loops:")
  val PatternResult(nestPattern, nestPatternCaps, _, _) = Pattern.compile("(a*b*)*")
  val nestProg = ProgramCompiler.compileRegexp(nestPattern, nestPatternCaps)
  val nestMatcher = new LinearRuntimeMatcher(nestProg)
  println(nestMatcher.matches("ab"))  // true
  println(nestMatcher.matches("aab")) // true
  println(nestMatcher.matches("aaabaaa")) // true
  println(nestMatcher.matches("b"))  // true
  println(nestMatcher.matches("ababc"))  // true
}
