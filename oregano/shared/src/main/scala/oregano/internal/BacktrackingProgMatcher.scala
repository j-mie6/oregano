/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import scala.quoted.*
import scala.annotation.tailrec

final class Backoffs(val width: Int, var count: Int, val parent: Backoffs | Null) {
    override def toString: String = s"Backoffs(width=$width, count=$count, parent=$parent)"
}

/*@tailrec
def forward(nextFn: Int => Int, pos: Int, b: Backoffs): (Backoffs, Int) = {
    //println(s"forward backoff: ${if b != null then b.toString else "null"}")
    val next = nextFn(pos)
    if (next == -1 || next == pos) (b, pos)
    else {
        val w = next - pos
        if b != null && b.width == w then {
            b.count += 1
            forward(nextFn, next, b)
        }
        else forward(nextFn, next, new Backoffs(w, 1, b))
    }
}

@tailrec
def backtrack(cur: Backoffs, startPos: Int, endPos: Int, i: Int, exitFn: Int => Int): Int = {
    //println(s"backtrack backoff: ${if cur != null then cur.toString else "null"}")
    if (cur == null) exitFn(startPos)
    else if (i < cur.count) {
        val attemptPos = endPos - i * cur.width
        val r = exitFn(attemptPos)
        if r >= 0 then r else backtrack(cur, startPos, endPos, i + 1, exitFn)
    }
    else backtrack(cur.parent, startPos, endPos, 0, exitFn)
}
*/


private object BacktrackingProgMatcher {
    // TODO: encapsulate this state properly
    private def compile(prog: Prog, pc: Int, end: Int, input: Expr[CharSequence], noCaps: Int, pos: Expr[Int], cap: Option[Expr[Array[Int]]], wholeMatch: Boolean)(using Quotes): Expr[Int] = {
        if (pc == end) pos
        else {
            val inst = prog.getInst(pc)
            inst.op match
                case InstOp.MATCH => cap match
                    case None => if wholeMatch then '{ if $pos == $input.length then $pos else -1 } else pos
                    case Some(cap) =>
                        if wholeMatch then '{
                            if ($pos == $input.length) {
                                $cap(1) = $pos
                                $pos
                            } else -1
                        }
                        else '{
                            $cap(1) = $pos
                            $pos
                        }
                case InstOp.FAIL => '{ -1 }

                case InstOp.ALT =>
                    val leftExpr = compile(prog, inst.out, end, input, noCaps, pos, cap, wholeMatch)
                    val rightExpr = compile(prog, inst.arg, end, input, noCaps, pos, cap, wholeMatch)
                    '{
                        val lp = $leftExpr
                        if lp >= 0 then lp else $rightExpr
                    }

                case InstOp.RUNE | InstOp.RUNE1 =>
                    val runeCheck = inst.matchRuneExpr
                    val nextPos = '{ $pos + 1 }
                    val succExpr = compile(prog, inst.out, end, input, noCaps, nextPos, cap, wholeMatch)
                    val charExpr: Expr[Int] = '{ $input.charAt($pos).toInt }
                    val condExpr: Expr[Boolean] = runeCheck(charExpr)

                    '{ if ($pos < $input.length && $condExpr) then $succExpr else -1 }

                case InstOp.LOOP => '{
                    def exit(pos: Int): Int = ${compile(prog, inst.arg, end, input, noCaps, 'pos, cap, wholeMatch)}

                    @tailrec
                    def forward(pos: Int, b: Backoffs | Null): (Backoffs | Null, Int) = {
                        val next = ${compile(prog, inst.out, pc, input, noCaps, 'pos, cap, wholeMatch)}

                        if (next == -1 || next == pos) (b, pos)
                        else {
                            val w = next - pos
                            if (b != null && b.width == w) {
                                b.count += 1
                                forward(next, b)
                            }
                            else forward(next, new Backoffs(w, 1, b))
                        }
                    }

                    @tailrec
                    def backtrack(cur: Backoffs | Null, base: Int, i: Int): Int = {
                        if (cur == null) exit($pos)
                        else if (i < cur.count) {
                            val attemptPos = base - i * cur.width
                            val r = exit(attemptPos)
                            if r >= 0 then r
                            else backtrack(cur, base, i + 1)
                        }
                        else backtrack(cur.parent, base, 0)
                    }

                    val (backoffs, finalPos) = forward($pos, null)
                    backtrack(backoffs, finalPos, 0)
                }

                case InstOp.CAPTURE =>
                    val slot = inst.arg
                    val nextExp = compile(prog, inst.out, end, input, noCaps, pos, cap, wholeMatch)

                    cap match {
                        case Some(cap) if slot < noCaps =>
                            val slotIdx: Expr[Int] = Expr(slot)
                            '{
                                val oldVal = $cap($slotIdx)
                                val curPos = $pos
                                val res = $nextExp
                                if (res >= 0) {
                                    $cap($slotIdx) = curPos
                                    res
                                }
                                else {
                                    $cap($slotIdx) = oldVal
                                    -1
                                }
                            }
                        case _ => '{
                            val res = $nextExp
                            if (res >= 0) then res else -1
                        }
                    }

                case _ => quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")
        }
    }

    def genMatcher(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] = '{ (input: CharSequence) =>
        val result: Int = ${compile(prog, prog.start, prog.numInst, 'input, 0, '{ 0 }, cap = None, wholeMatch = true)}
        result == input.length
    }

    def genMatcherWithCaps(prog: Prog)(using Quotes): Expr[CharSequence => Option[Array[Int]]] = '{ (input: CharSequence) =>
        val groups = Array.fill(${Expr(prog.numCap)})(-1)
        groups(0) = 0

        val result: Int = ${compile(prog, prog.start, prog.numInst, 'input, prog.numCap, '{ 0 }, cap = Some('groups), wholeMatch = true)}

        if result == input.length then Some(groups) else None
    }

    def genPrefixFind(prog: Prog)(using Quotes): Expr[(Int, CharSequence) => Int] = '{ (startPos: Int, input: CharSequence) =>
        /*val result: Int = */${compile(prog, prog.start, prog.numInst, 'input, 0, 'startPos, cap = None,  wholeMatch = false)}
        //result
    }

    // TODO: what is this for?
    /*def matches(prog: Prog, input: CharSequence): Boolean = {
        val cap = new Array[Int](prog.numCap)

        def compile(pc: Int, end: Int, pos: Int): Int = if (pc == end) pos else {
            val inst = prog.getInst(pc)
            inst.op match {
                case InstOp.MATCH =>
                    if (pos == input.length) {
                        cap(1) = pos // Capture full match end
                        pos
                    } else -1

                case InstOp.FAIL => -1
                case InstOp.ALT =>
                    val save = cap.clone()
                    val left = compile(inst.out, end, pos)
                    if (left >= 0) left
                    else {
                        Array.copy(save, 0, cap, 0, cap.length)
                        compile(inst.arg, end, pos)
                    }

                case InstOp.RUNE | InstOp.RUNE1 => if (pos < input.length && inst.matchRune(input.charAt(pos).toInt)) compile(inst.out, end, pos + 1) else -1

                case InstOp.LOOP =>
                    def loop(p: Int): Int = {
                        val save = cap.clone()
                        val next = compile(inst.out, pc, p)
                        if (next >= 0 && next != p) {
                            val inner = loop(next)
                            if (inner >= 0) inner
                            else {
                                Array.copy(save, 0, cap, 0, cap.length)
                                compile(inst.arg, end, p)
                            }
                        }
                        else compile(inst.arg, end, p)
                    }
                    loop(pos)

                case InstOp.CAPTURE =>
                    val slot = inst.arg
                    val nextPC = inst.out
                    if (slot % 2 == 0) {
                        val close = slot + 1
                        val oldStart = cap(slot)
                        val oldEnd = cap(close)
                        cap(slot) = pos
                        val result = compile(nextPC, end, pos)
                        if (result >= 0) {
                            cap(close) = result
                            result
                        }
                        else {
                            cap(slot) = oldStart
                            cap(close) = oldEnd
                            -1
                        }
                    } else {
                        val old = cap(slot)
                        cap(slot) = pos
                        val result = compile(nextPC, end, pos)
                        if (result < 0) cap(slot) = old
                        result
                    }

                case _ => throw new RuntimeException(s"Unsupported op: ${inst.op}")
            }
        }

        compile(prog.start, prog.numInst, 0) >= 0
    }*/
}
