package oregano.internal

final case class Thread(pc: Int, pos: Int)

class Matcher(prog: Prog) {
  case class Thread(pc: Int, pos: Int)

  def matches(input: CharSequence): Boolean = {
    var current = List(Thread(prog.start, 0))

    while current.nonEmpty do
      val next = scala.collection.mutable.ListBuffer.empty[Thread]

      for t <- current do
        if t.pos > input.length then
          ()
        else
          val inst = prog.getInst(t.pc)
          inst.op match
            case InstOp.FAIL =>
              ()

            case InstOp.MATCH =>
              if t.pos == input.length then return true

            case InstOp.NOP =>
              next += Thread(inst.out, t.pos)

            case InstOp.ALT =>
              next += Thread(inst.out, t.pos)
              next += Thread(inst.arg, t.pos)

            case InstOp.RUNE =>
              if t.pos < input.length && inst.matchRune(input.charAt(t.pos).toInt) then
                next += Thread(inst.out, t.pos + 1)

            case _ =>
              throw new IllegalArgumentException(s"Unsupported inst: $inst")

      current = next.toList
    false
  }
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
}