package oregano.internal

import oregano.internal.Pattern.Cat

final case class Frag( // Keep immutable to control sharing
    i: Int, // Instruction index
    out: Int = 0 // Patch list
)

class ProgramCompiler {
  private val progBuilder = new ProgBuilder()

  // always insert FAIL as first instruction
  progBuilder.addInst(InstOp.FAIL)

  def compileRegexp(re: Pattern, numCap: Int): Prog = {
    progBuilder.setNumCap(numCap * 2)
    val f = compile(re)
    progBuilder.patch(f.out, newInst(InstOp.MATCH).i)
    progBuilder.setStart(f.i)
    progBuilder.toProg
  }

  private def newInst(op: InstOp): Frag = {
    val pc = progBuilder.addInst(op)
    Frag(pc, 0)
  }

  private def nop(): Frag = {
    val f = newInst(InstOp.NOP)
    Frag(f.i, f.i << 1)
  }

  private def fail(): Frag = Frag(0, 0)

  private def cap(arg: Int): Frag = {
    val f = newInst(InstOp.CAPTURE)
    val inst = progBuilder.getInst(f.i)
    inst.arg = arg
    Frag(f.i, f.i << 1)
  }

  private def cat(f1: Frag, f2: Frag): Frag = {
    if f1.i == 0 || f2.i == 0 then return fail()
    progBuilder.patch(f1.out, f2.i)
    Frag(f1.i, f2.out)
  }

  private def alt(f1: Frag, f2: Frag): Frag = {
    if f1.i == 0 then return f2
    if f2.i == 0 then return f1
    val f = newInst(InstOp.ALT)
    val i = progBuilder.getInst(f.i)
    i.out = f1.i
    i.arg = f2.i
    val merged = progBuilder.append(f1.out, f2.out)
    Frag(f.i, merged)
  }

  private def star(f1: Frag, nongreedy: Boolean): Frag = {
    val f = newInst(InstOp.LOOP)
    val i = progBuilder.getInst(f.i)
    if nongreedy then
      i.arg = f1.i
      progBuilder.patch(f1.out, f.i)
      Frag(f.i, f.i << 1)
    else
      i.out = f1.i
      progBuilder.patch(f1.out, f.i)
      Frag(f.i, (f.i << 1) | 1)
  }

  /* 
  Below blindly reimplemented, perhaps useful for reference but currently unused.
   */
  // private def quest(f1: Frag, nongreedy: Boolean): Frag = {
  //   val f = newInst(InstOp.ALT)
  //   val i = progBuilder.getInst(f.i)
  //   val patchedOut = if nongreedy then
  //     i.arg = f1.i
  //     progBuilder.append(f.i << 1, f1.out)
  //   else
  //     i.out = f1.i
  //     progBuilder.append((f.i << 1) | 1, f1.out)
  //   Frag(f.i, patchedOut)
  // }

  // private def plus(f1: Frag, nongreedy: Boolean): Frag =
  //   Frag(f1.i, star(f1, nongreedy).out)

//   private def empty(op: Int): Frag = {
//     val f = newInst(InstOp.EMPTY_WIDTH)
//     prog.getInst(f.i).arg = op
//     Frag(f.i, f.i << 1)
//   }

  private def rune(r: Int, flags: Int): Frag =
    rune(Array(r), flags)

  private def rune(runes: Array[Int], flags0: Int): Frag = {
    val f = newInst(InstOp.RUNE)
    val inst = progBuilder.getInst(f.i)
    // var flags = flags0 & RE2.FOLD_CASE
    val flags = flags0
    // if (runes.length != 1 || Unicode.simpleFold(runes(0)) == runes(0))
    //   flags &= ~RE2.FOLD_CASE

    inst.runes = runes
    inst.arg = flags

    inst.op =
      //   if ((flags & RE2.FOLD_CASE) == 0 && runes.length == 1)
      // InstOp.RUNE1 else
      if (runes.length == 1)
        InstOp.RUNE1
      else if (runes.sameElements(Array(0, '\n' - 1, '\n' + 1, Utils.MAX_RUNE)))
        InstOp.RUNE_ANY_NOT_NL
      else if (runes.sameElements(Array(0, Utils.MAX_RUNE)))
        InstOp.RUNE_ANY
      else InstOp.RUNE

    Frag(f.i, f.i << 1)
  }

//   private val ANY_RUNE_NOT_NL = Array(0, '\n' - 1, '\n' + 1, Utils.MAX_RUNE)
//   private val ANY_RUNE = Array(0, Utils.MAX_RUNE)

  private def compile(re: Pattern): Frag = re match {
    case Pattern.Lit(c) =>
      rune(c, 0)
    case Pattern.Class(diet) =>
      // val runes = diet.toList.sorted
      val runes = Utils.dietToRanges(diet)
      val pairs: Array[Int] = runes
        .sliding(2, 2)
        .flatMap {
          case List(lo, hi)      => Seq(lo, hi)
          case List(single)      => Seq(single, single)
          case List(_, _, _, _*) => Seq.empty
          case Nil               => Seq.empty
        }
        .toArray
      rune(pairs, 0)

    case Pattern.Cat(Nil) =>
      nop()

    case Pattern.Cat(patterns) =>
      patterns.map(compile).reduce(cat)

    case Pattern.Alt(left, right) =>
      alt(compile(left), compile(right))

    case Pattern.Rep0(pat, _) =>
      val f = compile(pat)
      star(f, false)

    // case Pattern.Capture(idx, pat) => compile(pat)
    case Pattern.Capture(idx, pat) =>
      val bra = cap(idx << 1)
      val sub = compile(pat)
      val ket = cap(idx << 1 | 1)
      cat(cat(bra, sub), ket)
  }
}

object ProgramCompiler {
  def apply(): ProgramCompiler = new ProgramCompiler()

  def compileRegexp(re: Pattern, numCap: Int): Prog = {
    val compiler = new ProgramCompiler()
    compiler.compileRegexp(re, numCap)
  }
}

@main def testProgramCompiler(): Unit = {
  val regex = "(a(b))*c|def"
  val PatternResult(pattern, groupCount, _, _) = Pattern.compile(regex)
  // val pattern = patternResult.pattern
  // val numCap = patternResult.numGroups
  val frag = ProgramCompiler.compileRegexp(pattern, groupCount)
  println(frag)
}
