package oregano.internal

import scala.quoted.*

given ToExpr[Prog] with
  def apply(prog: Prog)(using Quotes): Expr[Prog] = {
    val instExprs: Expr[Seq[Inst]] =
      Expr.ofSeq(prog.insts.toSeq.map(Expr(_)))

    '{
      val instsArray: IArray[Inst] = IArray.from($instExprs)
      Prog(instsArray, ${ Expr(prog.start) }, ${ Expr(prog.numCap) })
    }
  }

final case class Prog(insts: IArray[Inst], start: Int, numCap: Int) {
  def getInst(pc: Int): Inst =
    insts(pc)

  def numInst: Int =
    insts.length

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

  def prefix: (String, Boolean) = {
    val sb = new StringBuilder
    var i = skipNop(start)

    if !InstOp.isRuneOp(i.op) || i.runes.length != 1 then
      return ("", i.op == InstOp.MATCH)

    while (
      InstOp.isRuneOp(i.op) &&
      i.runes.length == 1
    ) do
      appendCodePoint(sb, i.runes(0))
      i = skipNop(i.out)

    (sb.toString, i.op == InstOp.MATCH)
  }

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

  def next(l: Int): Int = {
    val i = insts(l >> 1)
    if (l & 1) == 0 then i.out else i.arg
  }

  override def toString: String = {
    insts.zipWithIndex.map { (inst, pc) =>
      val mark = if pc == start then "*" else ""
      f"$pc$mark%-3s  ${inst.toString}"
    }.mkString("\n")
  }
}
