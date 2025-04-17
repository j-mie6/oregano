package oregano.internal

import scala.quoted.*

/* 
ALT_MATCH unused in RE2J, could just nix it
 
Currently, uses enum as original: may be merit to moving to a case class? 
Main argument against (which I think is strong) is that mutability is nice for out/arg
*/

// TODO: move elsewhere?
// Escape everything but bread and butter ASCII, kind of a jackhammer 
// approach but who cares about stringification performance
private[internal] def escapeRune(sb: StringBuilder, r: Int): Unit = {
    if (r >= 32 && r <= 126 && r != '"') sb.append(r.toChar)
    else sb.append(f"\\u${r}%04x")
  }


given ToExpr[InstOp] with
  def apply(op: InstOp)(using Quotes): Expr[InstOp] = op match
    case InstOp.ALT              => '{ InstOp.ALT }
    case InstOp.ALT_MATCH        => '{ InstOp.ALT_MATCH }
    case InstOp.CAPTURE          => '{ InstOp.CAPTURE }
    case InstOp.EMPTY_WIDTH      => '{ InstOp.EMPTY_WIDTH }
    case InstOp.FAIL             => '{ InstOp.FAIL }
    case InstOp.MATCH            => '{ InstOp.MATCH }
    case InstOp.NOP              => '{ InstOp.NOP }
    case InstOp.RUNE             => '{ InstOp.RUNE }
    case InstOp.RUNE1            => '{ InstOp.RUNE1 }
    case InstOp.RUNE_ANY         => '{ InstOp.RUNE_ANY }
    case InstOp.RUNE_ANY_NOT_NL  => '{ InstOp.RUNE_ANY_NOT_NL }   
    case InstOp.LOOP             => '{ InstOp.LOOP }

enum InstOp:
    case ALT, ALT_MATCH, CAPTURE, EMPTY_WIDTH, FAIL, MATCH, NOP,
        RUNE, RUNE1, RUNE_ANY, RUNE_ANY_NOT_NL, LOOP

object InstOp:
  def isRuneOp(op: InstOp): Boolean =
    op.ordinal >= InstOp.RUNE.ordinal && op.ordinal <= InstOp.RUNE_ANY_NOT_NL.ordinal

given ToExpr[Inst] with
  def apply(inst: Inst)(using Quotes): Expr[Inst] =
    val runesExpr = Expr.ofList(inst.runes.toList.map(Expr(_)))
    '{
      new Inst(
        op    = ${ Expr(inst.op) },
        out   = ${ Expr(inst.out) },
        arg   = ${ Expr(inst.arg) },
        runes = $runesExpr.toArray
      )
    }

case class Inst(
  var op: InstOp,
  var out: Int = 0,
  var arg: Int = 0,
  var runes: Array[Int] = Array.empty
) {

  def matchRune(r: Int): Boolean =
    if runes.length == 1 then
      val r0 = runes(0)
    // TODO: deal with case folding
    //   if (arg & RE2.FOLD_CASE) != 0 then 
    //     Unicode.equalsIgnoreCase(r0, r)
    //   else 
      r == r0
    else
      // Fast check first few ranges
      var j = 0
      while j + 1 < runes.length && j <= 8 do
        if r < runes(j) then 
            return false
        if r <= runes(j + 1) then 
            return true
        j += 2

      // Binary search the remaining ranges
      var lo = 0
      var hi = runes.length / 2
      while lo < hi do
        val m = lo + (hi - lo) / 2
        val c = runes(2 * m)
        if c <= r then
          if r <= runes(2 * m + 1) then return true
          lo = m + 1
        else
          hi = m
      false

  override def toString: String = op match
    case InstOp.ALT         => s"alt -> $out, $arg"
    case InstOp.ALT_MATCH   => s"altmatch -> $out, $arg"
    case InstOp.CAPTURE     => s"cap $arg -> $out"
    case InstOp.EMPTY_WIDTH => s"empty $arg -> $out"
    case InstOp.FAIL        => "fail"
    case InstOp.MATCH       => "match"
    case InstOp.NOP         => s"nop -> $out"
    case InstOp.RUNE =>
    //   val suffix = if (arg & RE2.FOLD_CASE) != 0 then "/i" else "" TODO: deal with case folding
      val suffix = ""
      s"rune ${escapeRunes(runes)}$suffix -> $out"
    case InstOp.RUNE1 =>
      s"rune1 ${escapeRunes(runes)} -> $out"
    case InstOp.RUNE_ANY =>
      s"any -> $out"
    case InstOp.RUNE_ANY_NOT_NL =>
      s"anynotnl -> $out"
    case InstOp.LOOP =>
      s"loop -> $out, $arg"

  def matchRuneExpr(using Quotes): Expr[Int => Boolean] =
    if runes.length == 1 then
      val lit = runes(0)
      '{ (r: Int) => r == ${Expr(lit)} }

    else
      val pairs = runes.grouped(2).collect {
        case Array(lo, hi) => (lo, hi)
        case Array(single) => (single, single)
      }.toList

      if pairs.length <= 4 then
        '{
          (r: Int) => ${
            val conditions = pairs.map { case (lo, hi) =>
              if lo == hi then
                '{ r == ${Expr(lo)} }
              else
                '{ r >= ${Expr(lo)} && r <= ${Expr(hi)} }
            }
            conditions.reduceLeft((a, b) => '{ $a || $b })
          }
        }
      else
        // Fallback: binary search over ranges
        val pairsExpr = Expr.ofList(pairs.map { case (lo, hi) => Expr.ofTuple((Expr(lo), Expr(hi))) })
        '{
          val pairs = $pairsExpr
          (r: Int) =>
            var ret = false
            var lo = 0
            var hi = pairs.length
            while lo < hi do
              val m = lo + (hi - lo) / 2
              val (rlo, rhi) = pairs(m)
              if r < rlo then hi = m
              else if r > rhi then lo = m + 1
              ret = true
              lo = hi
            ret
        }

//   /** Returns true if this instruction matches (and consumes) the given rune. */
//   def matchRuneExpr(inst: Inst)(using Quotes): Expr[Int => Boolean] =
//     val runes = inst.runes

//     if runes.length == 1 then
//       val r0 = runes(0)
//       // Literal equality check
//       '{ (r: Int) => r == ${Expr(r0)} }

//     else
//       // Generate code for fast linear scan first, then fallback to binary search
//       val earlyCheckExprs = runes.grouped(2).take(4).map {
//         case Array(lo, hi) =>
//           '{ (r: Int) => r >= ${Expr(lo)} && r <= ${Expr(hi)} }
//         case _ => '{ (_: Int) => false }
//       }.toList

//       // Binary search expression
//       val pairs = runes.grouped(2).toArray
//       val binarySearchExpr =
//         '{
//           (r: Int) =>
//             var ret = false
//             var lo = 0
//             var hi = ${Expr(pairs.length)}
//             while lo < hi do
//               val m = lo + (hi - lo) / 2
//               val rlo = ${Expr(pairs)}(m)(0)
//               val rhi = ${Expr(pairs)}(m)(1)
//               if r < rlo then hi = m
//               else if (r > rhi) then lo = m + 1
//               else 
//                 ret = true
//                 break
//             ret
//         }

//       // Combine early exit with fallback binary search
//       val combined =
//         earlyCheckExprs.reduceLeftOption { (acc, next) =>
//           '{ (r: Int) => ${acc}(r) || ${next}(r) }
//         }.getOrElse('{ (_: Int) => false })

//       // Wrap in full check
//       '{
//         (r: Int) =>
//           if ${combined}(r) then true
//           else ${binarySearchExpr}(r)
//       }

  private def escapeRunes(runes: Array[Int]): String =
    val sb = new StringBuilder("\"")
    runes.foreach(escapeRune(sb, _))
    sb.append('"').toString()
}
