package oregano.internal

final case class Thread(pc: Int, pos: Int)

object Matcher {
  case class Thread(pc: Int, pos: Int)

  def matches(prog: Prog, input: CharSequence): Boolean =
    val inputLength = input.length

    // could look at using more terse datastructures, 
    // intuitively these are bound by PC (already finitely enumerated)
    // jamie suggested bitset, for example.
    var current     = scala.collection.mutable.ListBuffer.empty[Thread]
    var next        = scala.collection.mutable.ListBuffer.empty[Thread]
    var visited     = scala.collection.mutable.Set.empty[(Int, Int)]
    var nextVisited = scala.collection.mutable.Set.empty[(Int, Int)]

    inline def add(pc: Int, pos: Int): Unit =
        val key = (pc, pos)
        if !visited.contains(key) then
        visited += key
        current += Thread(pc, pos)

    add(prog.start, 0)

    while current.nonEmpty do
      next.clear()
      nextVisited.clear()

      var i = 0
        while i < current.size do
          val Thread(pc, pos) = current(i)
          val inst = prog.getInst(pc)
          inst.op match
            case InstOp.FAIL => ()

            case InstOp.MATCH =>
              if pos == inputLength then return true

            case InstOp.NOP =>
              val out = (inst.out, pos)
              if !visited.contains(out) && !nextVisited.contains(out) then
                  next += Thread(inst.out, pos)
                  nextVisited += out

            case InstOp.ALT | InstOp.LOOP =>
              val o = (inst.out, pos)
              val a = (inst.arg, pos)
              if !visited.contains(o) && !nextVisited.contains(o) then
                  next += Thread(inst.out, pos)
                  nextVisited += o
              if !visited.contains(a) && !nextVisited.contains(a) then
                  next += Thread(inst.arg, pos)
                  nextVisited += a

            case InstOp.RUNE | InstOp.RUNE1 =>
              if pos < inputLength && inst.matchRune(input.charAt(pos).toInt) then
                  val key = (inst.out, pos + 1)
                  if !visited.contains(key) && !nextVisited.contains(key) then
                  next += Thread(inst.out, pos + 1)
                  nextVisited += key

            case _ =>
              throw new RuntimeException(s"Unsupported op: ${inst.op}")
          i += 1

        current.clear()
        visited.clear()
        val tempVisited = nextVisited
        val tempCurrent = next
        nextVisited = visited
        next = current
        current = tempCurrent
        visited = tempVisited
    false
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