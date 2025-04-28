package oregano.internal

import scala.quoted.*

inline def makeMatch(): Int => Int = ${ makeMatchImpl }

def makeMatchImpl(using Quotes): Expr[Int => Int] =
  import quotes.reflect.*

  val lambdaSym = Symbol.spliceOwner
  val methodType = MethodType(List("x"))(
    _ => List(TypeRepr.of[Int]),
    _ => TypeRepr.of[Int]
  )

  Lambda(lambdaSym, methodType, (owner, args) => {
    val xTree = args.head
    val xTerm = xTree.asInstanceOf[Term]
    val xExpr = xTerm.asExprOf[Int]

    val case1 = CaseDef(
      Literal(IntConstant(1)),
      None,
      '{ $xExpr + $xExpr }.asTerm
    )

    val defaultCase = CaseDef(
      Wildcard(),
      None,
      Literal(IntConstant(10))
    )

    Match(xTerm, List(case1, defaultCase))
  }).asExprOf[Int => Int]

// val f = makeMatch()
// println(f(1))  // 2
// println(f(5))  // 10