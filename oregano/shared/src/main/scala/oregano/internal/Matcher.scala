package oregano.internal

final case class Thread(pc: Int, pos: Int)

class Matcher(prog: Prog) {
  case class Thread(pc: Int, pos: Int)

  def matches(input: CharSequence): Boolean =
    val inputLength = input.length

    val current     = scala.collection.mutable.ListBuffer.empty[Thread]
    val next        = scala.collection.mutable.ListBuffer.empty[Thread]
    val visited     = scala.collection.mutable.Set.empty[(Int, Int)]
    val nextVisited = scala.collection.mutable.Set.empty[(Int, Int)]

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

            case InstOp.ALT =>
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
        current ++= next
        visited.clear()
        visited ++= nextVisited

    false
}

@main def vmTest: Unit = {
  println("Basic alternation:")
  val basicPattern = Pattern.compile("a|b")
  val basicProg = ProgramCompiler.compileRegexp(basicPattern)
  val basicMatcher = new Matcher(basicProg)

  println(basicMatcher.matches("a")) // true
  println(basicMatcher.matches("b")) // true
  println(basicMatcher.matches("c")) // false
  println(basicMatcher.matches(""))  // false

  println("\nConcatenation:")
  val concatPattern = Pattern.compile("ab")
  val concatProg = ProgramCompiler.compileRegexp(concatPattern)
  val concatMatcher = new Matcher(concatProg)
  println(concatMatcher.matches("ab"))  // true
  println(concatMatcher.matches("a"))   // false
  println(concatMatcher.matches("abc")) // false

  println("\nLiteral + alternation:")
  val complex = Pattern.compile("ab|cd")
  val complexProg = ProgramCompiler.compileRegexp(complex)
  val complexMatcher = new Matcher(complexProg)
  println(complexMatcher.matches("ab")) // true
  println(complexMatcher.matches("cd")) // true
  println(complexMatcher.matches("ac")) // false
  println(complexMatcher.matches("abcd")) // false

  println("\nCharacter class:")
  val classPattern = Pattern.compile("[a-z]")
  val classProg = ProgramCompiler.compileRegexp(classPattern)
  val classMatcher = new Matcher(classProg)
  println(classMatcher.matches("a"))  // true
  println(classMatcher.matches("z"))  // true
  println(classMatcher.matches("A"))  // false
  println(classMatcher.matches("0"))  // false

  println("\nKleene Star:")
  val starPattern = Pattern.compile("a*b")
  val starProg = ProgramCompiler.compileRegexp(starPattern)
  val starMatcher = new Matcher(starProg)
  println(starMatcher.matches("ab"))  // true
  println(starMatcher.matches("aab")) // true
  println(starMatcher.matches(""))   // false
  println(starMatcher.matches("b"))  // true
}