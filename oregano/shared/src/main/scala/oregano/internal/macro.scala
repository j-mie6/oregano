package oregano.internal

import scala.quoted.*

private [oregano] def compileMacro[S <: String & Singleton: Type](s: S)(using Quotes): Expr[oregano.Regex[S]] = {
    import quotes.reflect.report
    val ast = parse(s)
    report.info(ast.toString)
    '{???}
}
