package oregano.internal

class NoCapThread(var inst: Inst)

object NoCapThread:
  def apply(n: Int): NoCapThread = new NoCapThread(null)

class NoCapQueue(n: Int):
  val denseThreads: Array[NoCapThread] = new Array[NoCapThread](n)
  val densePcs: Array[Int] = new Array[Int](n)
  val sparse: Array[Int] = new Array[Int](n)
  var size: Int = 0

  def contains(pc: Int): Boolean =
    val j = sparse(pc)
    j < size && densePcs(j) == pc

  def isEmpty: Boolean = size == 0

  def add(pc: Int): Int =
    val j = size
    sparse(pc) = j
    denseThreads(j) = null
    densePcs(j) = pc
    size += 1
    j

  def getThread(pc: Int): NoCapThread =
    val j = sparse(pc)
    if j < size && densePcs(j) == pc then denseThreads(j)
    else null

  def clearThread(pc: Int): Unit =
    val j = sparse(pc)
    if j < size && densePcs(j) == pc then
      denseThreads(j) = null

  def clear(): Unit = size = 0


class NoCapMachine(val prog: Prog):
  val q0 = new NoCapQueue(prog.numInst)
  val q1 = new NoCapQueue(prog.numInst)
  var pool: Array[NoCapThread] = Array.fill(10)(NoCapThread(prog.numCap))
  var poolSize: Int = pool.length
  var matched: Boolean = false

  def alloc(inst: Inst): NoCapThread =
    val t =
      if poolSize > 0 then
        poolSize -= 1
        pool(poolSize)
      else new NoCapThread(inst)
    t.inst = inst
    t

  def free(q: NoCapQueue): Unit = free(q, 0)

  def free(q: NoCapQueue, from: Int): Unit =
    val needed = poolSize + (q.size - from)
    if pool.length < needed then
      pool = Array.copyOf(pool, math.max(pool.length * 2, needed))

    for i <- from until q.size do
      val t = q.denseThreads(i)
      if t != null then
        pool(poolSize) = t
        poolSize += 1
    q.clear()

  def free(t: NoCapThread): Unit =
    if pool.length <= poolSize then
      pool = Array.copyOf(pool, pool.length * 2)
    pool(poolSize) = t
    poolSize += 1

  def add(pc: Int, pos: Int, q: NoCapQueue, t: NoCapThread): NoCapThread =
    if pc == 0 then return t
    if q.contains(pc) then return t

    val d = q.add(pc)
    val inst = prog.insts(pc)

    inst.op match
      case InstOp.FAIL => t

      case InstOp.NOP | InstOp.CAPTURE =>
        add(inst.out, pos, q, t)

      case InstOp.ALT | InstOp.LOOP =>
        val t1 = add(inst.out, pos, q, t)
        val t2 = add(inst.arg, pos, q, t1)
        t2

      case InstOp.MATCH | InstOp.RUNE | InstOp.RUNE1 | InstOp.RUNE_ANY | InstOp.RUNE_ANY_NOT_NL =>
        val thread = if t == null then alloc(inst) else { t.inst = inst; t }
        q.denseThreads(d) = thread
        null

      case _ =>
        throw new IllegalStateException(s"bad inst: ${inst.op}")


  def step(
    runq: NoCapQueue,
    nextq: NoCapQueue,
    pos: Int,
    rune: Int,
  ): Boolean =
    var matchedHere = false
    var j = 0

    while j < runq.size do
      var t = runq.denseThreads(j)
      if t != null then
        val inst = t.inst

        // Longest-match pruning
        if matched then
          free(t)
        else
          var addNext = false
          inst.op match
            case InstOp.MATCH =>
              matchedHere = true

            case InstOp.RUNE =>
              addNext = inst.matchRune(rune)

            case InstOp.RUNE1 =>
              addNext = rune == inst.runes(0)

            case InstOp.RUNE_ANY =>
              addNext = true

            case InstOp.RUNE_ANY_NOT_NL =>
              addNext = rune != '\n'

            case _ =>
              throw new IllegalStateException(s"bad inst: ${inst.op}")

          if addNext then
            t = add(inst.out, pos + 1, nextq, t)

          if t != null then
            free(t)
            runq.denseThreads(j) = null

      j += 1

    runq.clear()
    matchedHere

  def matches(input: CharSequence): Boolean =
    var runq = q0
    var nextq = q1
    var pos = 0
    matched = false

    add(prog.start, 0, runq, null)

    while !matched && !runq.isEmpty do
      // println(s"$pos")
      val rune =
        if pos < input.length then input.charAt(pos).toInt
        else -1 // EOF

      matched = step(runq, nextq, pos, rune)

      // Only advance position if not at EOF
      if pos < input.length then pos += 1
      val tmp = runq; runq = nextq; nextq = tmp

    matched

@main def testNoCapMachine(): Unit =
  val PatternResult(nestPattern, nestPatternCaps, _, _) = Pattern.compile("((a)*b*)*")
  val prog = ProgramCompiler.compileRegexp(nestPattern, nestPatternCaps)
  val machine = NoCapMachine(prog)
  println(machine.matches("aaabaaa"))
