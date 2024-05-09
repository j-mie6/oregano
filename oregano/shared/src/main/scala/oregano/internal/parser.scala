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
    private val keyChars = Set('(', ')', '{', '}', '[', '.', '*', '+', '?', '\\', '|', '$', '^')
    //private val escapableChars = keyChars ++ Set('<', '>', ',', '&')

    lazy val regex = expr <~ eof
    // technically, they can be empty on either side of this... we need an Epsilon
    private lazy val expr = chain.right1(term)(Alt from '|')
    private lazy val term = Cat(some(lit | (Dot from '.') | cls))
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
    private lazy val lit = Lit(noneOf(keyChars).map(_.toInt) | charEsc)
    // I believe these two can always appear together, are ambiguous, and `charEsc` should always be first, so make it atomic
    private lazy val charEsc: Parsley[Int] = atomic(empty)
    private lazy val setEsc: Parsley[Diet[Int]] = empty

    lazy val cls = Class(clsSet)
    private lazy val clsSet: Parsley[Diet[Int]] = '[' ~> ('^' ~> clsBody.map(Regex.AllSet -- _) | clsBody)  <~ ']'
    // classes may not be empty, and ] can be used as part of one in that instance: []] is ], but [a]] is a] and [] is an error
    // although []a] is also treated as `a|]`...
    private lazy val clsBody = clsIntersect | ']' ~> clsIntersect
    private lazy val clsAtom = noneOf(']', '[', '\\', '&').map(_.toInt) | atomic('&'.map(_.toInt) <~ notFollowedBy('&')) | charEsc
    private lazy val clsRange = clsAtom.zip(option(atomic('-' ~> clsAtom))).mapFilterMsg {
        case (l, Some(r)) if l < r => Right(Diet.fromRange(Range(l, r)))
        case (l, Some(r)) => Left(Seq(s"ranges must be ascending, but '$l' is greater than '$r'")) //TODO: whitespace in message!
        case (l, None) => Right(Diet.one(l))
    } | setEsc
    private lazy val clsUnion = (clsRange | clsSet).reduceLeft(_ | _)
    // intersection is lowest precedence, but it's a bit of a pain, because [&&X] and [X&&] are legal, but [&&] is not.
    // similarly, [X&&..&&Y] is the same as [X&&Y].
    private lazy val clsIntersect = sepBy1(option(clsUnion), "&&").mapFilterMsg { css => css.flatten.reduceOption(_ & _) match
        case Some(cs) => Right(cs)
        case None => Left(Seq("class intersections cannot be empty on both sides"))
    }

    // need to make these atomic in general
    private given Conversion[String, Parsley[String]] = str => atomic(string(str))
}

private def parse(str: String) = parsers.regex.parse(str).toEither
