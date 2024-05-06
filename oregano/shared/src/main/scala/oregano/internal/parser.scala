package oregano.internal

import parsley.quick.*
import parsley.expr.*
import parsley.syntax.character.*
import Regex.*

private object parsers {
    // FIXME: these sets are non-exhaustive right now
    private val keyChars = Set('(', ')', '{', '}', '[', ']', '.', '*', '+', '?', '\\', '|', '$', '^')
    private val escapableChars = keyChars ++ Set('<', '>', ',', '&')

    lazy val regex = expr <~ eof
    // technically, they can be empty on either side of this... we need an Epsilon
    private lazy val expr = chain.right1(term)(Alt from '|')
    private lazy val term = Cat(some(lit | (Dot from '.')))
    /** `x`: the character x.
      * `\\`: the backslash character.
      * `\0n`: the octal character with value n (0 <= n <= 7).
      * `\0nn`: the octal character with value nn (0 <= n <= 7).
      * `\0mnn`: the octal character with value mnn (0 <= m <= 3, 0 <= n <= 7).
      * `\xhh`: the hexadecimal value 0xhh.
      * `\xhhhh: the hexadecimal value 0xhhhh.
      * `\x{h..h}`: the hexadecimal value of arbitrary digits up to MAX_CODE_POINT.
      * `\t`: the tab character.
      * `\n`: the newline character.
      * `\r`: the carriage-return character.
      * `\f`: the form-feed character.
      * `\a`: the ascii bell.
      * `\e`: the escape character (u001B).
      * `\cx`: the control character corresponding to x (@-?) (skipping space)
      */
    private lazy val lit = Lit(noneOf(keyChars) /* TODO: escape chars! */)
    /** `[abc]`: a, b, or c.
      * `[^abc]`: any character except for a, b, or c.
      * `[a-zA-Z]`: a through z or A through Z, inclusive.
      * `[a-z[A-Z]]`: alias for the above.
      * `[a-z&&a]`: just a.
      * `[a-z&&[def]]`: intersection (d, e, or f).
      * `[a-z&&[^bc]]`: equivalent to `[ad-z]`.
      * `[a-z&&[^m-p]]`: equivalent to `[a-lq-z]`.
      */
}

private def parse(str: String) = parsers.regex.parse(str).toEither
