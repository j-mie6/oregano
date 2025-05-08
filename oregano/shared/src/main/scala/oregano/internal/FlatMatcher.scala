package oregano.internal

import scala.quoted.*
import scala.annotation.switch

// a lot of this is stupid, I wanted to try having a function 
// that didn't need to initialise DS before calling, I think that is the way forward
final class FlatTable(
  val n: Int,
  val start: Int,
  val op: Array[Int],
  val out: Array[Int],
  val arg: Array[Int],
  val runes: Array[Int => Boolean]
)

object FlatTable:
  def build(prog: Prog)(using Quotes): Expr[FlatTable] =
    val nExpr = Expr(prog.numInst)
    val startExpr = Expr(prog.start)

    val opExprs = (0 until prog.numInst).map(i => Expr(prog.getInst(i).op.ordinal))
    val outExprs = (0 until prog.numInst).map(i => Expr(prog.getInst(i).out))
    val argExprs = (0 until prog.numInst).map(i => Expr(prog.getInst(i).arg))

    val runeExprs = (0 until prog.numInst).map { i =>
      val inst = prog.getInst(i)
      if inst.op == InstOp.RUNE || inst.op == InstOp.RUNE1 then
        inst.matchRuneExpr  // Expr[Int=>Boolean]
      else
        '{ (_: Int) => false }
    }

    '{ 
      new FlatTable(
        $nExpr,
        $startExpr,
        Array(${ Varargs(opExprs) }*),
        Array(${ Varargs(outExprs)}*),
        Array(${ Varargs(argExprs)}*),
        Array(${ Varargs(runeExprs)}*)
      )
    }

final class FlatQueue(n: Int):
  val sparse = new Array[Int](n)
  val densePcs = new Array[Int](n)
  val densePos = new Array[Int](n)
  var size = 0

  def isEmpty: Boolean = size == 0

  def contains(pc: Int): Boolean =
    val j = sparse(pc)
    j < size && densePcs(j) == pc

  def add(pc: Int, pos: Int): Unit =
    val j = size
    sparse(pc) = j
    densePcs(j) = pc
    densePos(j) = pos
    size += 1

  def clear(): Unit =
    size = 0

object FlatMatcher:
  def genMatcherFlat(prog: Prog)(using Quotes): Expr[(CharSequence, FlatTable) => Boolean] =
    '{
      (input: CharSequence, table: FlatTable) =>
        val length = input.length
        var runq  = FlatQueue(table.n)
        var nextq = FlatQueue(table.n)
        var matched = false

        runq.add(table.start, 0)

        while !matched && !runq.isEmpty do
          var i = 0
          while i < runq.size do
            val pc  = runq.densePcs(i)
            val pos = runq.densePos(i)
            i += 1
            // ugly to use switch for performance
            (table.op(pc): @switch) match
              case 5 => // MATCH
                if pos == length then 
                  matched = true

              case 7 | 8 => // RUNE, RUNE1
                if pos < length && table.runes(pc)(input.charAt(pos).toInt) then
                  nextq.add(table.out(pc), pos + 1)

              case 6 => // NOP
                runq.add(table.out(pc), pos)

              case 0 | 11 => // ALT, LOOP
                runq.add(table.out(pc), pos)
                runq.add(table.arg(pc), pos)

              case _ => ()

          runq.clear()
          val tmp = runq; runq = nextq; nextq = tmp

        matched
    }

  def matches(input: CharSequence, table: FlatTable): Boolean = {
    val length = input.length
    var runq  = FlatQueue(table.n)
    var nextq = FlatQueue(table.n)
    var matched = false

    runq.add(table.start, 0)

    while (!matched && !runq.isEmpty) {
      var i = 0
      while (i < runq.size) {
        val pc  = runq.densePcs(i)
        val pos = runq.densePos(i)
        i += 1

        // table.op(pc) match {
        //   case x if x == InstOp.MATCH.ordinal =>
        //     if (pos == length) matched = true

        //   case x if x == InstOp.RUNE.ordinal || x == InstOp.RUNE1.ordinal =>
        //     if (pos < length && table.runes(pc)(input.charAt(pos).toInt))
        //       nextq.add(table.out(pc), pos + 1)

        //   case x if x == InstOp.NOP.ordinal =>
        //     runq.add(table.out(pc), pos)

        //   case x if x == InstOp.ALT.ordinal || x == InstOp.LOOP.ordinal =>
        //     runq.add(table.out(pc), pos)
        //     runq.add(table.arg(pc), pos)

        //   case x if x == InstOp.FAIL.ordinal =>
        //     () // do nothing
          
        //   case _ =>
        //     throw new RuntimeException(s"Unexpected opcode: ${table.op(pc)}")
        // }
        /* 
        as it stands, it seems likely that Prog will stay live, 
        may be worth examining the implications of matching on Prog, 
        this is naturally ugly (but performant)!
         */
        (table.op(pc): @switch) match
          case 5 => // MATCH
            if pos == length then matched = true

          case 7 | 8 => // RUNE, RUNE1
            if pos < length && table.runes(pc)(input.charAt(pos).toInt) then
              nextq.add(table.out(pc), pos + 1)

          case 6 => // NOP
            runq.add(table.out(pc), pos)

          case 0 | 11 => // ALT, LOOP
            runq.add(table.out(pc), pos)
            runq.add(table.arg(pc), pos)

          case _ => ()
      }

      runq.clear()
      val tmp = runq; runq = nextq; nextq = tmp
    }

    matched
  }
