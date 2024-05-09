/*
 * Copyright 2022 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import parsley.quick.*
import parsley.expr.*
import parsley.syntax.character.charLift
import parsley.errors.combinator.*

import cats.collections.{Diet, Range}

import scala.language.implicitConversions

import Regex.*

private object parsers {
    // FIXME: these sets are non-exhaustive right now
    // Also, turns out that ] is legal outside of a char class?!
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
      * `\cx`: the control character corresponding to x (@-?) -- space is somehow valid for this, but don't know what to
      */
    private lazy val lit = Lit(noneOf(keyChars) | charEsc)
    // I believe these two can always appear together, are ambiguous, and `charEsc` should always be first, so make it atomic
    private lazy val charEsc: Parsley[Char] = atomic(empty)
    private lazy val setEsc: Parsley[Diet[Char]] = empty
    /** `[abc]`: a, b, or c.
      * `[^abc]`: any character except for a, b, or c.
      * `[a-zA-Z]`: a through z or A through Z, inclusive.
      * `[a-z[A-Z]]`: alias for the above.
      * `[a-z&&a]`: just a.
      * `[a-z&&[def]]`: intersection (d, e, or f).
      * `[a-z&&[^bc]]`: equivalent to `[ad-z]`.
      * `[a-z&&[^m-p]]`: equivalent to `[a-lq-z]`.
      *
      * The precedence of character class operators is as follows:
      *   1. literal escape
      *   2. grouping
      *   3. range
      *   4. union
      *   5. intersection
      *
      * the set of metacharacters is ,[\ with &^]- soft-metacharacters
      */
    lazy val cls = Class(clsSet)

    private lazy val clsSet = '[' ~> ('^' ~> clsBody.map(Regex.AllSet -- _) | clsBody)  <~ ']'.explain("classes may not be empty")
    // classes may not be empty, and ] can be used as part of one in that instance: []] is ], but [a]] is a] and [] is an error
    // although []a] is also treated as `a|]`...
    private lazy val clsBody = clsIntersect | ']' ~> clsIntersect

    private lazy val clsAtom: Parsley[Char] = noneOf(']', '[', '\\', '&') | atomic('&' <~ notFollowedBy('&')) | charEsc
    private lazy val clsRange: Parsley[Diet[Char]] = clsAtom.zip(option(atomic('-' ~> clsAtom))).mapFilterMsg {
        case (l, Some(r)) if l < r => Right(Diet.fromRange(Range(l, r)))
        case (l, Some(r)) => Left(Seq(s"ranges must be ascending, but '$l' is greater than '$r'")) //TODO: whitespace in message!
        case (l, None) => Right(Diet.one(l))
    } | setEsc
    private lazy val clsUnion: Parsley[Diet[Char]] = (clsRange | clsSet).reduceLeft(_ | _)

    // intersection is lowest precedence, but it's a bit of a pain, because [&&X] and [X&&] are legal, but [&&] is not.
    // similarly, [X&&..&&Y] is the same as [X&&Y].
    private lazy val clsIntersect = sepBy1(option(clsUnion), "&&").mapFilterMsg { ocss =>
        val css = ocss.flatten
        if (css.nonEmpty) Right(css.reduce(_ & _))
        else Left(Seq("class intersections cannot be empty on both sides"))
    }

    // need to make these atomic in general
    private given Conversion[String, Parsley[String]] = str => atomic(string(str))
}

private def parse(str: String) = parsers.regex.parse(str).toEither
