/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import parsley.generic.*
import cats.collections.{Diet, Range}

// This is matching with Java 8 Regex
// TODO: not all constructs can appear within other constructs... perhaps this can be
// broken into a few layers?
private enum Regex {
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
    case Lit(c: Int)
    /** `[abc]`: a, b, or c.
      * `[^abc]`: any character except for a, b, or c.
      * `[a-zA-Z]`: a through z or A through Z, inclusive.
      * `[a-z[A-Z]]`: alias for the above.
      * `[a-z&&a]`: just a.
      * `[a-z&&[def]]`: intersection (d, e, or f).
      * `[a-z&&[^bc]]`: equivalent to `[ad-z]`.
      * `[a-z&&[^m-p]]`: equivalent to `[a-lq-z]`.
      */
    case Class(cs: Diet[Int]) // TODO: needs a smart-constructor for internal character classes
    /** matches any character (this can include line terminators depending on flags) */
    case Dot extends Regex with ParserBridge0[Regex]
    /** `^`: the beginning of a line */
    case LineStart extends Regex with ParserBridge0[Regex]
    /** `$`: the end of a line */
    case LineEnd extends Regex with ParserBridge0[Regex]
    /** `\b`: a word boundary */
    case WordBoundary extends Regex with ParserBridge0[Regex]    // FIXME: is this just a composite?
    /** `\B`: a non-word boundary */
    case NonWordBoundary extends Regex with ParserBridge0[Regex] // FIXME: is this just a composite?
    /** `\A`: beginning of the input */
    case InputStart extends Regex with ParserBridge0[Regex]
    /** `\G`: end of previous match */
    case PrevMatchEnd extends Regex with ParserBridge0[Regex]
    /** `\Z`: end of input except for final terminator, if any */
    case InputEndSansFinal extends Regex with ParserBridge0[Regex]
    /** `\z`: end of input */
    case InputEnd extends Regex with ParserBridge0[Regex]
    /** `X?`: once or not at all */
    case Opt(r: Regex)
    /** `X*`: zero or more times */
    case Rep0(r: Regex)
    /** `X+`: one or more times */
    case Rep1(r: Regex)
    /** `X{n}: exactly n times */
    case Exactly(r: Regex, n: Int)
    /** `X{n,}: at least n times */
    case AtLeast(r: Regex, n: Int)
    /** `X{n,m}: at least n times, but more than m times */
    case Between(r: Regex, n: Int, m: Int)
    /** `X??`: once or not at all */
    case LazyOpt(r: Regex)
    /** `X*?`: zero or more times */
    case LazyRep0(r: Regex)
    /** `X+?`: one or more times */
    case LazyRep1(r: Regex)
    /** `X{n}?: exactly n times */
    case LazyExactly(r: Regex, n: Int)
    /** `X{n,}?: at least n times */
    case LazyAtLeast(r: Regex, n: Int)
    /** `X{n,m}?: at least n times, but more than m times */
    case LazyBetween(r: Regex, n: Int, m: Int)
    /** `XY`: X followed by Y */
    case Cat(rs: List[Regex])
    /** `X|Y`: Either X or Y */
    case Alt(r1: Regex, r2: Regex)
    /** `(X)`: X, as a capturing group */
    case Capture(r: Regex)
    /** `(?:X)`: X, as a non-capturing group */
    case NonCapture(r: Regex) 
    /** `\n`: whatever the nth capturing group matched */
    case Back(n: Int)
    /** `\k<name>`: whatever the named group name matched */
    case NamedBack(name: String)
    /** `(?<name>X)`: named capturing group */
    case NamedCapture(name: String, r: Regex)
    // TODO: flag set, and flag set within
    /** `(?=X)`: zero-width positive lookahead */
    case AheadPos(r: Regex)
    /** `(?!X)`: zero-width negative lookahead */
    case AheadNeg(r: Regex)
    /** `(?<=X)`: zero-width positive lookbehind */
    case BehindPos(r: Regex)
    /** `(?<!X)`: zero-width negative lookbehind */
    case BehindNeg(r: Regex)
    /** `(?>X)`: an atomic X, which does not engage in backtracking "only-once subexpression" */
    case Atomic(r: Regex)
    // NOTE: \Q, \E, and (?:X) are not represented in syntax
}
private object Regex {
    private [internal] val AllSet = Diet.fromRange(Range(0x00000, 0x1ffff))
    object Lit extends ParserBridge1[Int, Regex] {
        override def labels: List[String] = List("literal")
    }
    object Class extends ParserBridge1[Diet[Int], Regex]
    object Opt extends ParserBridge1[Regex, Regex]
    object Rep0 extends ParserBridge1[Regex, Regex]
    object Rep1 extends ParserBridge1[Regex, Regex]
    object Exactly extends ParserBridge2[Regex, Int, Regex]
    object AtLeast extends ParserBridge2[Regex, Int, Regex]
    object Between extends ParserBridge3[Regex, Int, Int, Regex]
    object LazyOpt extends ParserBridge1[Regex, Regex]
    object LazyRep0 extends ParserBridge1[Regex, Regex]
    object LazyRep1 extends ParserBridge1[Regex, Regex]
    object LazyExactly extends ParserBridge2[Regex, Int, Regex]
    object LazyAtLeast extends ParserBridge2[Regex, Int, Regex]
    object LazyBetween extends ParserBridge3[Regex, Int, Int, Regex]
    object Cat extends ParserBridge1[List[Regex], Regex] {
        def apply(rs: Regex*): Regex = Cat(rs.toList)
    }
    object Alt extends ParserBridge2[Regex, Regex, Regex]
    object Capture extends ParserBridge1[Regex, Regex]
    object Back extends ParserBridge1[Int, Regex]
    object NamedBack extends ParserBridge1[String, Regex]
    object NamedCapture extends ParserBridge2[String, Regex, Regex]
    object AheadPos extends ParserBridge1[Regex, Regex]
    object AheadNeg extends ParserBridge1[Regex, Regex]
    object BehindPos extends ParserBridge1[Regex, Regex]
    object BehindNeg extends ParserBridge1[Regex, Regex]
    object Atomic extends ParserBridge1[Regex, Regex]

    // Predefined Character Classes
    /** `\d`: a digit */
    object Digit extends ParserSingletonBridge[Class] {
        def con = new Class(set)
        def set = Diet.fromRange(Range('0'.toInt, '9'.toInt))
    }

    /** `\D`: a non-digit */
    object NonDigit extends ParserSingletonBridge[Class] {
        def con = new Class(AllSet -- Digit.set)
    }

    /** `\h`: horizontal whitespace */
    object HorizontalWhitespace extends ParserSingletonBridge[Class] {
        def con = new Class(set)
        def set = Diet.one(' '.toInt)
                      .add('\t'.toInt)
                      .add(0x00a0)
                      .add(0x1680)
                      .add(0x180e)
                      .addRange(Range(0x2000, 0x200a))
                      .add(0x202f)
                      .add(0x205f)
                      .add(0x3000)
    }

    /** `\H`: non-horizontal whitespace */
    object NonHorizontalWhitespace extends ParserSingletonBridge[Class] {
        def con = new Class(AllSet -- HorizontalWhitespace.set)
    }

    /** `\s`: a whitespace character **/
    object Whitespace extends ParserSingletonBridge[Class] {
        def con = new Class(set)
        def set = Diet.one(' '.toInt)
                      .add('\t'.toInt)
                      .add('\n'.toInt)
                      .add(0x000b)
                      .add('\f'.toInt)
                      .add('\r'.toInt)
    }

    /** `\S`: not a whitespace character */
    object NonWhitespace extends ParserSingletonBridge[Class] {
        def con = new Class(AllSet -- Whitespace.set)
    }

    /** `\v`: a whitespace character **/
    object VerticalWhitespace extends ParserSingletonBridge[Class] {
        def con = new Class(set)
        def set = Diet.one('\n'.toInt)
                      .add(0x000b)
                      .add('\f'.toInt)
                      .add('\r'.toInt)
                      .add(0x0085)
                      .add(0x2028)
                      .add(0x2029)
    }

    /** `\V`: not a whitespace character */
    object NonVerticalWhitespace extends ParserSingletonBridge[Class] {
        def con = new Class(AllSet -- VerticalWhitespace.set)
    }

    /** `\w`: a word character */
    object Word extends ParserSingletonBridge[Class] {
        def con = new Class(set)
        def set = Diet.fromRange(Range('a'.toInt, 'z'.toInt))
                      .addRange(Range('A'.toInt, 'Z'.toInt))
                      .add('_'.toInt)
                      .addRange(Range('0'.toInt, '9'.toInt))
    }

    /** `\W`: a non-word character */
    object NonWord extends ParserSingletonBridge[Class] {
        def con = new Class(AllSet -- Word.set)
    }

    //TODO: POSIX Character Classes
    /*
    \p{Lower}	A lower-case alphabetic character: [a-z]
    \p{Upper}	An upper-case alphabetic character:[A-Z]
    \p{ASCII}	All ASCII:[\x00-\x7F]
    \p{Alpha}	An alphabetic character:[\p{Lower}\p{Upper}]
    \p{Digit}	A decimal digit: [0-9]
    \p{Alnum}	An alphanumeric character:[\p{Alpha}\p{Digit}]
    \p{Punct}	Punctuation: One of !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
    \p{Graph}	A visible character: [\p{Alnum}\p{Punct}]
    \p{Print}	A printable character: [\p{Graph}\x20]
    \p{Blank}	A space or a tab: [ \t]
    \p{Cntrl}	A control character: [\x00-\x1F\x7F]
    \p{XDigit}	A hexadecimal digit: [0-9a-fA-F]
    \p{Space}	A whitespace character: [ \t\n\x0B\f\r]
    */

    //TODO: java.lang.Character Classes
    /*
    \p{javaLowerCase}	Equivalent to java.lang.Character.isLowerCase()
    \p{javaUpperCase}	Equivalent to java.lang.Character.isUpperCase()
    \p{javaWhitespace}	Equivalent to java.lang.Character.isWhitespace()
    \p{javaMirrored}	Equivalent to java.lang.Character.isMirrored()
    */

    //TODO: Classes for Unicode Scripts, Blocks, Categories, and Binary Properties
    /*
    \p{IsLatin}			A Latin script character (script)
    \p{InGreek}			A character in the Greek block (block)
    \p{Lu}				An uppercase letter (category)
    \p{IsAlphabetic}	An alphabetic character (binary property)
    \p{Sc}				A currency symbol
    \P{InGreek}			Any character except one in the Greek block (negation)
    [\p{L}&&[^\p{Lu}]]	Any letter except an uppercase letter (subtraction)
    */

    /** `\R`: any Unicode linebreak sequence */
    object Linebreak extends ParserSingletonBridge[Regex] {
        def con = Alt(Cat(Lit(0x000d), Lit(0x000a)), Class(set))
        def set = Diet.fromRange(Range(0x000a, 0x000d))
                      .add(0x0085)
                      .add(0x2028)
                      .add(0x2029)
    }
}
