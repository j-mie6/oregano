package oregano.internal

import scala.quoted.*
import cats.collections.Diet

object Utils {
  def countNodes(expr: Expr[Any])(using Quotes): Int =
    import quotes.reflect.*
    // println(expr.show)

    def loop(tree: Tree): Int = 
      tree match
        case Inlined(_, _, body) => 1 + loop(body)
        case Block(stats, expr) => 1 + stats.map(loop).sum + loop(expr)
        case Apply(fun, args) => 1 + loop(fun) + args.map(loop).sum
        case TypeApply(fun, args) => 1 + loop(fun) + args.map(loop).sum
        case Select(qualifier, _) => 1 + loop(qualifier)
        case Ident(_) => 1
        case Literal(_) => 1
        case If(cond, thenp, elsep) => 1 + loop(cond) + loop(thenp) + loop(elsep)
        case Match(selector, cases) => 1 + loop(selector) + cases.map {
                                              case CaseDef(_, guard, rhs) =>
                                                1 + guard.map(loop).getOrElse(0) + loop(rhs)
                                            }.sum
        case ValDef(_, _, rhsOpt) => 1 + rhsOpt.map(loop).getOrElse(0)
        case DefDef(_, _, _, bodyOpt) => 1 + bodyOpt.map(loop).getOrElse(0)
        case Typed(expr, _) => 1 + loop(expr)
        case This(_) => 1
        case New(_) => 1
        case While(cond, body) => 1 + loop(cond) + loop(body)
        case Return(expr, _) => 1 + loop(expr)
        case Assign(lhs, rhs) => 1 + loop(lhs) + loop(rhs)
        case Repeated(elems, _) => 1 + elems.map(loop).sum
        case Lambda(_, body) => 1 + loop(body)
        case Closure(meth, tpeOpt) => 1 + loop(meth) + tpeOpt.map(_ => 1).getOrElse(0)
        case _: TypeTree => 1
        case _ => 
          // This is essentially to flag if I missed something: this function has been grokked from staring at Scala compiler source code!
          println(s"Unhandled tree type: ${tree.getClass.getSimpleName}")
          1  // final catch-all for safety

    loop(expr.asTerm)

  inline def dietToRanges(diet: Diet[Int]): List[Int] = {
    diet.toIterator.foldLeft(List.empty[Int]) { (acc, range) =>
      acc :+ range.start :+ range.end
    }.toList
  }

  inline val MAX_RUNE = 0x10FFFF // Maximum valid Unicode code point
}

