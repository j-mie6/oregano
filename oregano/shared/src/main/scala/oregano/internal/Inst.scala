/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import scala.quoted.*

/*
ALT_MATCH unused in RE2J, could just nix it

Currently, uses enum as original: may be merit to moving to a case class?
Main argument against (which I think is strong) is that mutability is nice for out/arg
 */

private enum InstOp {
    case ALT, ALT_MATCH, CAPTURE, EMPTY_WIDTH, FAIL, MATCH, NOP, RUNE, RUNE1, RUNE_ANY, RUNE_ANY_NOT_NL, LOOP
    def isRuneOp: Boolean = ordinal >= InstOp.RUNE.ordinal && ordinal <= InstOp.RUNE_ANY_NOT_NL.ordinal
}
private object InstOp {
    given ToExpr[InstOp] {
        def apply(op: InstOp)(using Quotes): Expr[InstOp] = op match
        case InstOp.ALT             => '{InstOp.ALT}
        case InstOp.ALT_MATCH       => '{InstOp.ALT_MATCH}
        case InstOp.CAPTURE         => '{InstOp.CAPTURE}
        case InstOp.EMPTY_WIDTH     => '{InstOp.EMPTY_WIDTH}
        case InstOp.FAIL            => '{InstOp.FAIL}
        case InstOp.MATCH           => '{InstOp.MATCH}
        case InstOp.NOP             => '{InstOp.NOP}
        case InstOp.RUNE            => '{InstOp.RUNE}
        case InstOp.RUNE1           => '{InstOp.RUNE1}
        case InstOp.RUNE_ANY        => '{InstOp.RUNE_ANY}
        case InstOp.RUNE_ANY_NOT_NL => '{InstOp.RUNE_ANY_NOT_NL}
        case InstOp.LOOP            => '{InstOp.LOOP}
    }
}

private final case class Inst(op: InstOp, out: Int, arg: Int, runes: IArray[Int]) {
    // TODO: rewrite, tail recursive
    def matchRune(r: Int): Boolean = {
        if (runes.length == 1) {
            val r0 = runes(0)
            // TODO: deal with case folding
            //   if (arg & RE2.FOLD_CASE) != 0 then
            //     Unicode.equalsIgnoreCase(r0, r)
            //   else
            r == r0
        }
        else {
            // Fast check first few ranges
            var j = 0
            while (j + 1 < runes.length && j <= 8) {
                if r < runes(j) then return false
                if r <= runes(j + 1) then return true
                j += 2
            }
            // Binary search the remaining ranges
            var lo = 0
            var hi = runes.length / 2
            while (lo < hi) {
                val m = lo + (hi - lo) / 2
                val c = runes(2 * m)
                if (c <= r) {
                    if (r <= runes(2 * m + 1)) return true
                    lo = m + 1
                }
                else hi = m
            }
            false
        }
    }

    override def toString: String = op match
        case InstOp.ALT             => s"alt -> $out, $arg"
        case InstOp.ALT_MATCH       => s"altmatch -> $out, $arg"
        case InstOp.CAPTURE         => s"cap $arg -> $out"
        case InstOp.EMPTY_WIDTH     => s"empty $arg -> $out"
        case InstOp.FAIL            => "fail"
        case InstOp.MATCH           => "match"
        case InstOp.NOP             => s"nop -> $out"
        case InstOp.RUNE            =>
            //   val suffix = if (arg & RE2.FOLD_CASE) != 0 then "/i" else "" TODO: deal with case folding
            val suffix = ""
            s"rune ${escapeRunes(runes)}$suffix -> $out"
        case InstOp.RUNE1           => s"rune1 ${escapeRunes(runes)} -> $out"
        case InstOp.RUNE_ANY        => s"any -> $out"
        case InstOp.RUNE_ANY_NOT_NL => s"anynotnl -> $out"
        case InstOp.LOOP            => s"loop -> $out, $arg"

    def matchRuneExpr(using Quotes): Expr[Int] => Expr[Boolean] = {
        if (runes.length == 1) {
            val lit = runes(0)
            (r: Expr[Int]) => '{ $r == ${Expr(lit)} }
        }
        else {
            val pairs: List[(Int, Int)] = runes.grouped(2).collect {
                case IArray(lo, hi) => (lo, hi)
                case IArray(single) => (single, single)
            }.toList

            (r: Expr[Int]) => {
                val conditions = pairs.map { (lo, hi) =>
                    if lo == hi then '{ $r == ${Expr(lo)} } else '{ $r >= ${Expr(lo)} && $r <= ${Expr(hi)} }
                }
                conditions.reduceLeft((a, b) => '{ $a || $b })
            }
        }
    }

    private def escapeRunes(runes: IArray[Int]): String = {
        val sb = new StringBuilder("\"")
        for (r <- runes) {
            // Escape everything but bread and butter ASCII, kind of a jackhammer
            // approach but who cares about stringification performance for printing
            if (r >= 32 && r <= 126 && r != '"') sb.append(r.toChar)
            else sb.append(f"\\u${r}%04x")
        }
        sb.append('"').toString
    }
}
private object Inst {
    given ToExpr[Inst] {
        def apply(inst: Inst)(using Quotes): Expr[Inst] = {
            val runesExpr = Expr.ofSeq(inst.runes.toList.map(Expr(_)))
            '{new Inst(op = ${ Expr(inst.op) }, out = ${ Expr(inst.out) }, arg = ${ Expr(inst.arg) }, runes = IArray($runesExpr*)) }
        }
    }
}

private case class MutableInst(
    var op: InstOp,
    var out: Int = 0,
    var arg: Int = 0,
    var runes: Array[Int] = Array.empty
) {
  def toInst: Inst = Inst(op, out, arg, IArray.from(runes))
}
