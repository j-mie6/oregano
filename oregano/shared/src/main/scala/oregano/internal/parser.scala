/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import parsley.quick.*
import parsley.expr.*
import parsley.syntax.character.charLift
import parsley.errors.combinator.*
import parsley.syntax.all.*

import cats.collections.{Diet, Range}

import scala.language.implicitConversions

import Regex.*

private object parsers {
    // FIXME: these sets are non-exhaustive right now
    private val keyChars = Set('(', ')', '{', '}', '[', '.', '*', '+', '?', '\\', '|', '$', '^')
    //private val escapableChars = keyChars ++ Set('<', '>', ',', '&')

    lazy val regex = expr <~ eof
    // technically, they can be empty on either side of this... we need an Epsilon
    private lazy val expr: Parsley[Regex] = chain.right1(term)(Alt from '|')
    private lazy val term = Cat(some(chain.postfix(atom)(postfixOps)))
    private lazy val atom = nonCapture | capture | lit | (Dot from '.') | cls
    private lazy val nonCapture = atomic(string("(?:")) ~> expr <~ ')' map NonCapture.apply
    private lazy val capture = '(' ~> expr <~ ')' map Capture.apply
    private lazy val lit = Lit(noneOf(keyChars).map(_.toInt) | charEsc)
    // I believe these two can always appear together, are ambiguous, and `charEsc` should always be first, so make it atomic
    private lazy val charEsc: Parsley[Int] = {
        // corresponds with \x{ ... }
        val hexArbEscX = hexDigit.foldLeft1[BigInt](0)((n, d) => n * 16 + d.asDigit).collectMsg("characters cannot exceed largest codepoint 0x1ffff") {
            case n if n <= Character.MAX_CODE_POINT => n.toInt
        }
        // corresponds with \xhh
        val hexFixedEscX = (hexDigit, hexDigit).zipped {
            case (d1, d2) => d1.asDigit * 16 + d2.asDigit
        }
        // corresponds with \uHHHH
        val hexFixedEscU = (hexDigit, hexDigit, hexDigit, hexDigit).zipped {
            case (d1, d2, d3, d4) => d1.asDigit * 4096 + d2.asDigit * 256 + d3.asDigit * 16 + d4.asDigit 
        }
        val hexCodeEscX = '{' ~> hexArbEscX <~ '}' | hexFixedEscX
        val octCode = range(min=1, max=3)(octDigit).mapFilterMsg { ds =>
            val n = ds.foldLeft(0)((n, d) => n * 8 + d.asDigit)
            if (n > 255) Left(Seq("octal escape sequences cannot be greater than 0377 (255 in decimal)"))
            else Right(n)
        }
        val numeric = 'x' ~> hexCodeEscX | '0' ~> octCode | 'u' ~> hexFixedEscU
        // `\cx`: the control character corresponding to x (@-?) -- space is somehow valid for this, but don't know what to
        val control = 'c' ~> empty
        val single  = choice(Map('t' -> 0x00009, 'n' -> 0x0000a, 'r' -> 0x0000d, 'f' -> 0x0000c, 'a' -> 0x00007, 'e' -> 0x0001b).toList.map(_ as _)*)
        atomic('\\' ~> (single | numeric | control | '\\'.map(_.toInt)))
    }
    private lazy val setEsc: Parsley[Diet[Int]] = empty

    lazy val cls = {
        lazy val clsSet: Parsley[Diet[Int]] = '[' ~> ('^' ~> clsBody.map(Regex.AllSet -- _) | clsBody)  <~ ']'
        // classes may not be empty, and ] can be used as part of one in that instance: []] is ], but [a]] is a] and [] is an error
        // although []a] is also treated as `a|]`...
        lazy val clsBody = clsIntersect | ']' ~> clsIntersect
        lazy val clsAtom = noneOf(']', '[', '\\', '&').map(_.toInt) | atomic('&'.map(_.toInt) <~ notFollowedBy('&')) | charEsc
        lazy val clsRange = clsAtom.zip(option(atomic('-' ~> clsAtom))).mapFilterMsg {
            case (l, Some(r)) if l < r => Right(Diet.fromRange(Range(l, r)))
            case (l, Some(r)) => Left(Seq(s"ranges must be ascending, but '$l' is greater than '$r'")) //TODO: whitespace in message!
            case (l, None) => Right(Diet.one(l))
        } | setEsc
        lazy val clsUnion = (clsRange | clsSet).reduceLeft(_ | _)
        // intersection is lowest precedence, but it's a bit of a pain, because [&&X] and [X&&] are legal, but [&&] is not.
        // similarly, [X&&..&&Y] is the same as [X&&Y].
        lazy val clsIntersect = sepBy1(option(clsUnion), "&&").mapFilterMsg { css => css.flatten.reduceOption(_ & _) match
            case Some(cs) => Right(cs)
            case None => Left(Seq("class intersections cannot be empty on both sides"))
        }
        Class(clsSet)
    }
    lazy val postfixOps = '*' #> Rep0.apply <|> '+' #> Rep1.apply

    // need to make these atomic in general
    private given Conversion[String, Parsley[String]] = str => atomic(string(str))
}

private def parse(str: String) = parsers.regex.parse(str).toEither
