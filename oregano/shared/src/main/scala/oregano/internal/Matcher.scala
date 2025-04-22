package oregano.internal

// may want something mutable, so can preallocate instead
// note that re2j stores an inst, one less layer of indirection
// case class Thread(pc: Int, pos: Int) 

final case class Thread(inst: Inst, pos: Int) 

private final class ThreadQueue(n: Int):
  val sparse = new Array[Int](n)
  val densePcs = new Array[Int](n)
  val denseThreads = new Array[Option[Thread]](n)
  var size = 0

  def isEmpty: Boolean = size == 0

  def contains(pc: Int): Boolean =
    val j = sparse(pc)
    j < size && densePcs(j) == pc

  def add(pc: Int): Int =
    val j = size
    sparse(pc) = j
    densePcs(j) = pc
    denseThreads(j) = None
    size += 1
    j

  def clear(): Unit =
    size = 0

  def getThread(i: Int): Option[Thread] = denseThreads(i)
  def setThread(i: Int, t: Thread): Unit = denseThreads(i) = Some(t)

  override def toString: String =
    densePcs.take(size).mkString("{", ", ", "}")

object Matcher {
  def matches(prog: Prog, input: CharSequence): Boolean =
    val inputLength = input.length
    var runq = ThreadQueue(prog.numInst)
    var nextq = ThreadQueue(prog.numInst)

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
            q.setThread(id, Thread(inst, pos)) // optional: if storing Thread object

    def step(runq: ThreadQueue, nextq: ThreadQueue): Boolean =
      var i = 0
      while i < runq.size do
        runq.getThread(i) match
          case Some(Thread(inst, pos)) =>
            inst.op match
              case InstOp.MATCH =>
                if pos == inputLength then return true

              case InstOp.RUNE | InstOp.RUNE1 =>
                if pos < inputLength && inst.matchRune(input.charAt(pos).toInt) then
                  add(nextq, inst.out, pos + 1)

              case _ =>
                throw new RuntimeException(s"Unexpected opcode in step: ${inst.op}")
          case None => ()
        i += 1
      false

    add(runq, prog.start, 0)

    var matched = false
    while !matched && !runq.isEmpty do
      matched = step(runq, nextq)
      runq.clear()
      val tmp = runq
      runq = nextq
      nextq = tmp

    matched
}

@main def vmTest: Unit = {
  println("Basic alternation:")
  val basicPattern = Pattern.compile("a|b")
  val basicProg = ProgramCompiler.compileRegexp(basicPattern)

  println(Matcher.matches(basicProg, "a")) // true
  println(Matcher.matches(basicProg, "b")) // true
  println(Matcher.matches(basicProg, "c")) // false
  println(Matcher.matches(basicProg, ""))  // false

  println("\nConcatenation:")
  val concatPattern = Pattern.compile("ab")
  val concatProg = ProgramCompiler.compileRegexp(concatPattern)
  println(Matcher.matches(concatProg, "ab"))  // true
  println(Matcher.matches(concatProg, "a"))   // false
  println(Matcher.matches(concatProg, "abc")) // false

  println("\nLiteral + alternation:")
  val complex = Pattern.compile("ab|cd")
  val complexProg = ProgramCompiler.compileRegexp(complex)
  println(Matcher.matches(complexProg, "ab")) // true
  println(Matcher.matches(complexProg, "cd")) // true
  println(Matcher.matches(complexProg, "ac")) // false
  println(Matcher.matches(complexProg, "abcd")) // false

  println("\nCharacter class:")
  val classPattern = Pattern.compile("[a-z]")
  val classProg = ProgramCompiler.compileRegexp(classPattern)
  println(Matcher.matches(classProg, "a"))  // true
  println(Matcher.matches(classProg, "z"))  // true
  println(Matcher.matches(classProg, "A"))  // false
  println(Matcher.matches(classProg, "0"))  // false

  println("\nKleene Star:")
  val starPattern = Pattern.compile("(a|b)*ab")
  val starProg = ProgramCompiler.compileRegexp(starPattern)
  println(Matcher.matches(starProg, "ab"))  // true
  println(Matcher.matches(starProg, "aab")) // true
  println(Matcher.matches(starProg, ""))   // false
  println(Matcher.matches(starProg, "b"))  // true
}