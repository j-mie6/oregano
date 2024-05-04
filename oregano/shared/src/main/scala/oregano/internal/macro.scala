package oregano.internal

import scala.quoted.*

private [oregano] def compileMacro(s: String)(using Quotes): Expr[oregano.Regex[?]] = {
    import quotes.reflect.report
    val ast = parse(s)
    report.info(ast.toString)
    '{???}
}
