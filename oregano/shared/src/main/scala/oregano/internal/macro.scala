package oregano.internal

import scala.quoted.*

private [oregano] def compileMacro(s: String)(using Quotes): Expr[oregano.Regex[?]] = {
    import quotes.reflect.report
    parse(s) match
    case Right(ast) =>
        report.info(s"$ast")
        '{
            // just do compile-time checks for now!
            new oregano.Regex[List[String]] {
                val regex = ${Expr(s)}.r
                def matches(input: CharSequence): Boolean = regex.matches(input)
                def unapplySeq(input: CharSequence): Option[List[String]] = regex.unapplySeq(input)
            }
        }
    case Left(err) => report.errorAndAbort(err)
}
