package oregano.internal

/* 
A Scala port of the RE2J machine, currently to help me reason about things
*/
class RE2Thread(var inst: Inst, var cap: Array[Int])

object RE2Thread:
  def apply(n: Int): RE2Thread = new RE2Thread(null, new Array[Int](n))

class RE2Queue(n: Int):
  val denseThreads: Array[RE2Thread] = new Array[RE2Thread](n)
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

  def getThread(pc: Int): RE2Thread =
    val j = sparse(pc)
    if j < size && densePcs(j) == pc then denseThreads(j)
    else null

  def clearThread(pc: Int): Unit =
    val j = sparse(pc)
    if j < size && densePcs(j) == pc then
      denseThreads(j) = null

  def clear(): Unit = size = 0

  def getCap(pc: Int): Array[Int] =
    denseThreads(sparse(pc)).cap

class RE2Machine(val prog: Prog):
  val q0 = new RE2Queue(prog.numInst)
  val q1 = new RE2Queue(prog.numInst)
  var pool: Array[RE2Thread] = Array.fill(10)(RE2Thread(prog.numCap))
  var poolSize: Int = pool.length
  var matched: Boolean = false
  var matchcap: Array[Int] = new Array[Int](prog.numCap)
  var ncap: Int = prog.numCap


  def init(ncap: Int): Unit =
    this.ncap = ncap
    if (ncap > matchcap.length) then
      pool.foreach(t => t.cap = new Array[Int](ncap))
      matchcap = new Array[Int](ncap)
    else
      for i <- 0 until poolSize do
        val t = pool(i)
        if t != null then
          t.cap = new Array[Int](ncap)
          pool(i) = t

  def submatches(): Array[Int] =
    if ncap == 0 then Array.empty[Int]
    else Array.copyOf(matchcap, ncap)

  def alloc(inst: Inst): RE2Thread =
    val t =
      if poolSize > 0 then
        poolSize -= 1
        pool(poolSize)
      else new RE2Thread(inst, new Array[Int](matchcap.length))
    t.inst = inst
    t

  def free(q: RE2Queue): Unit = free(q, 0)

  def free(q: RE2Queue, from: Int): Unit =
    val needed = poolSize + (q.size - from)
    if pool.length < needed then
      pool = Array.copyOf(pool, math.max(pool.length * 2, needed))

    for i <- from until q.size do
      val t = q.denseThreads(i)
      if t != null then
        pool(poolSize) = t
        poolSize += 1
    q.clear()

  def free(t: RE2Thread): Unit =
    if pool.length <= poolSize then
      pool = Array.copyOf(pool, pool.length * 2)
    pool(poolSize) = t
    poolSize += 1

  def add(pc: Int, pos: Int, cap: Array[Int], q: RE2Queue, t: RE2Thread): RE2Thread =
    if pc == 0 then return t
    if q.contains(pc) then return t

    val d = q.add(pc)
    val inst = prog.insts(pc)

    inst.op match
      case InstOp.FAIL => t

      case InstOp.NOP =>
        add(inst.out, pos, cap, q, t)

      case InstOp.ALT | InstOp.LOOP =>
        val t1 = add(inst.out, pos, cap, q, t)
        val t2 = add(inst.arg, pos, cap, q, t1)
        t2

      case InstOp.CAPTURE =>
        if inst.arg < ncap then
          val old = cap(inst.arg)
          cap(inst.arg) = pos
          add(inst.out, pos, cap, q, null)
          cap(inst.arg) = old
          t
        else
          add(inst.out, pos, cap, q, t)

      case InstOp.MATCH | InstOp.RUNE | InstOp.RUNE1 | InstOp.RUNE_ANY | InstOp.RUNE_ANY_NOT_NL =>
        val thread = if t == null then alloc(inst) else { t.inst = inst; t }
        if ncap > 0 && (thread.cap ne cap) then
          System.arraycopy(cap, 0, thread.cap, 0, ncap)
        q.denseThreads(d) = thread
        null

      case _ =>
        throw new IllegalStateException(s"bad inst: ${inst.op}")


  def step(
    runq: RE2Queue,
    nextq: RE2Queue,
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
        if matched && ncap > 0 && matchcap(0) < t.cap(0) then
          free(t)
        else
          var addNext = false
          // println(s"step at pos=$pos, char=${rune}, inst=${inst}, cap=${t.cap.mkString(",")}")
          inst.op match
            case InstOp.MATCH =>
              // Anchoring checks skipped; assume unanchored for now
              if ncap > 0 && (!matched || matchcap(1) < pos) then
                t.cap(1) = pos
                System.arraycopy(t.cap, 0, matchcap, 0, ncap)
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
            t = add(inst.out, pos + 1, t.cap, nextq, t)

          if t != null then
            free(t)
            runq.denseThreads(j) = null

      j += 1

    runq.clear()
    matchedHere

  def stepWithTable(
    addTable: Array[AddFn],
    runq: RE2Queue,
    nextq: RE2Queue,
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
        if matched && ncap > 0 && matchcap(0) < t.cap(0) then
          free(t)
        else
          var addNext = false
          // println(s"step at pos=$pos, char=${rune}, inst=${inst}, cap=${t.cap.mkString(",")}")
          inst.op match
            case InstOp.MATCH =>
              // Anchoring checks skipped; assume unanchored for now
              if ncap > 0 && (!matched || matchcap(1) < pos) then
                t.cap(1) = pos
                System.arraycopy(t.cap, 0, matchcap, 0, ncap)
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
            t = addTable(inst.out)(this, pos + 1, inst.out, t.cap, nextq, t)

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
    matchcap.indices.foreach(i => matchcap(i) = 0)

    add(prog.start, 0, matchcap.clone(), runq, null)

    while !matched && !runq.isEmpty do
      // println(s"$pos")
      val rune =
        if pos < input.length then input.charAt(pos).toInt
        else -1 // EOF

      matched = step(runq, nextq, pos, rune)

      // Only advance position if not at EOF
      if pos < input.length then pos += 1
      val tmp = runq; runq = nextq; nextq = tmp

    // free(runq) // redundant, we aren't short circuiting
    // println(matchcap.mkString(" "))
    matched

  def matchesWithTable(input: CharSequence, addTable: Array[AddFn]): Boolean =
    var runq  = q0
    var nextq = q1
    var pos   = 0
    matched   = false
    java.util.Arrays.fill(matchcap, 0)

    // initialise first thread
    addTable(prog.start)(this, 0, prog.start, matchcap.clone(), runq, null)

    while !matched && !runq.isEmpty do
      val rune = if pos < input.length then input.charAt(pos).toInt else -1
      matched = stepWithTable(addTable, runq, nextq, pos, rune)
      if pos < input.length then pos += 1
      val tmp = runq; runq = nextq; nextq = tmp
    // println(matchcap.mkString(" "))
    matched
