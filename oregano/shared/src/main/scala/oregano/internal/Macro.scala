package oregano.internal

import scala.quoted.*

private [oregano] def compileMacro[S <: String & Singleton: Type](s: S)(using Quotes): Expr[oregano.Regex[S]] = {
    val ast = parse(s)
    '{???}
}

private def parse(str: String): Option[Regex] = None
