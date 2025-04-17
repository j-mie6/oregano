// package oregano.internal

// import scala.quoted.*

// object VMCodegenCPS:

//   def genMatcher(prog: Prog)(using Quotes): Expr[CharSequence => Boolean] =

//     val map = scala.collection.mutable.Map[Int, Expr[Int => Int]]()

//     def compileCPS(pc: Int, input: Expr[CharSequence], k: Expr[Int] => Expr[Int])(using Quotes): Expr[Int => Int] =
//       map.getOrElseUpdate(pc, {
//         val inst = prog.getInst(pc)
//         inst.op match
//           case InstOp.MATCH =>
//             '{ (pos: Int) => if pos == $input.length then ${k('pos)} else -1 }

//           case InstOp.FAIL =>
//             '{ (_: Int) => -1 }

//           case InstOp.NOP =>
//             compileCPS(inst.out, input, k)

//           case InstOp.ALT =>
//             val left  = compileCPS(inst.out, input, k)
//             val right = compileCPS(inst.arg, input, k)
//             '{ 
//               (pos: Int) =>
//               val lp = $left(pos)
//               if lp >= 0 then lp else $right(pos)  
//             }

//           case InstOp.RUNE | InstOp.RUNE1 =>
//             val runeCheck = inst.matchRuneExpr
//             val outExpr = compileCPS(inst.out, input, k)
//             '{ 
//               (pos: Int) =>
//                 if pos < $input.length && $runeCheck($input.charAt(pos).toInt)
//                 then $outExpr(pos + 1)
//                 else -1
//             }

//           case InstOp.LOOP =>
//             // Build loop body first, with continuation being the loop itself
//             // Declare lazy val first so we can reference it
//             lazy val loopExpr: Expr[Int => Int] = '{
//               def loop(pos: Int): Int = {
//                 val next = ${ compileCPS(inst.out, input, pos => '{ loop($pos) }) }(pos)
//                 if next >= 0 then loop(next)
//                 else ${compileCPS(inst.arg, input, k)}(pos)
//               }
//               loop
//             }

//             loopExpr

//           case _ =>
//             quotes.reflect.report.errorAndAbort(s"Unsupported op: ${inst.op}")
//       })

//     // Final wrapper expression
//     '{
//       (input: CharSequence) =>
//         val matcher = ${ compileCPS(prog.start, 'input, p => p ) }
//         matcher(0) >= 0
//     }