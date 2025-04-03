package oregano.internal

import scala.collection.mutable.ArrayBuffer
import scala.quoted.*

given ToExpr[Prog] with
  def apply(prog: Prog)(using Quotes): Expr[Prog] = {
    val instList: List[Inst] = prog.insts.take(prog.numInst).toList
    val instExprs: Expr[List[Inst]] = Expr.ofList(instList.map(Expr(_)))
    
    '{
      val p = new Prog()
      // Rebuild the mutable ArrayBuffer from the lifted list
      p.insts = ArrayBuffer.from($instExprs)
      p.start = ${ Expr(prog.start) }
      p.numCap = ${ Expr(prog.numCap) }
      p
    }
  }

final class Prog {
  var insts = ArrayBuffer.empty[Inst]
  var start: Int = 0
  var numCap: Int = 2 // default: full match group $0

  def getInst(pc: Int): Inst =
    insts(pc)

  def numInst: Int =
    insts.length

// origially void, might not need ret
  def addInst(op: InstOp): Int = {
    val i = Inst(op)
    insts += i
    insts.length - 1
  }

  /** Follows NOPs and CAPTUREs to real instruction */
  def skipNop(pc: Int): Inst = {
    var i = insts(pc)
    var currentPc = pc
    while i.op == InstOp.NOP || i.op == InstOp.CAPTURE do
      currentPc = i.out
      i = insts(currentPc)
    i
  }

  def appendCodePoint(sb: StringBuilder, codePoint: Int): Unit =
    if Character.isBmpCodePoint(codePoint) then
        sb.append(codePoint.toChar)
    else
        sb.append(Character.highSurrogate(codePoint))
        sb.append(Character.lowSurrogate(codePoint))


  /** Extracts literal prefix, returns (prefix, isExactMatch) */
  def prefix: (String, Boolean) = {
    val sb = new StringBuilder
    var i = skipNop(start)

    if !InstOp.isRuneOp(i.op) || i.runes.length != 1 then
      return ("", i.op == InstOp.MATCH)

    while (
        InstOp.isRuneOp(i.op) 
        && i.runes.length == 1 
        // && (i.arg & RE2.FOLD_CASE) == 0
        ) do
      appendCodePoint(sb, i.runes(0))
      i = skipNop(i.out)

    (sb.toString, i.op == InstOp.MATCH)
  }

  /** Returns leading EMPTY_WIDTH flags required at match start, or -1 if unreachable */
  // TODO: not sure if this is correct
  def startCond: Int = {
    var flag = 0
    var pc = start

    while true do
      val i = insts(pc)
      i.op match
        case InstOp.EMPTY_WIDTH => flag |= i.arg; pc = i.out
        case InstOp.FAIL        => return -1
        case InstOp.CAPTURE | InstOp.NOP => pc = i.out
        case _ => return flag
    flag 
  }

  // --------------------------
  // Patch list support
  // --------------------------

  /** Computes next pointer in a patch list */
  def next(l: Int): Int = {
    val i = insts(l >> 1)
    if (l & 1) == 0 then i.out else i.arg
  }

  /** Patches all locations in patch list `l` with destination `value`. */
  def patch(l: Int, value: Int): Unit = {
    var cursor = l
    while cursor != 0 do
      val i = insts(cursor >> 1)
      val next = if (cursor & 1) == 0 then
        val tmp = i.out
        i.out = value
        tmp
      else
        val tmp = i.arg
        i.arg = value
        tmp
      cursor = next
  }

  /** Appends patch list `l2` to the end of `l1` and returns the merged list */
  def append(l1: Int, l2: Int): Int = {
    if l1 == 0 then return l2
    if l2 == 0 then return l1

    var last = l1
    var nextVal = next(last)
    while nextVal != 0 do
      last = nextVal
      nextVal = next(last)

    val i = insts(last >> 1)
    if (last & 1) == 0 then i.out = l2
    else i.arg = l2

    l1
  }

  override def toString: String = {
    insts.zipWithIndex.map { (inst, pc) =>
      val mark = if pc == start then "*" else ""
      f"$pc$mark%-3s  ${inst.toString}"
    }.mkString("\n")
  }
}

object Prog {
  def apply(): Prog = new Prog()
}
