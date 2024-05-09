/*
 * Copyright 2022 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import cats.collections.{Diet, Range}

import parsley.{Success, Failure}

class ParserTests extends AnyFlatSpec {
    import parsers.*
    import Regex.*
    "regex classes" should "parse sets of literals" in {
        cls.parse("[abc]") shouldBe Success(Class(Diet.fromRange(Range('a', 'c'))))
        cls.parse("[.]") shouldBe Success(Class(Diet.one('.')))
        cls.parse("[^abc]") shouldBe Success(Class(Diet.fromRange(Range('\u0000', '`')).addRange(Range('d', '\uffff'))))
        cls.parse("[0^]") shouldBe Success(Class(Diet.one('0').add('^')))
        cls.parse("[&]") shouldBe Success(Class(Diet.one('&')))
    }

    they should "reject empty sets" in {
        cls.parse("[]") shouldBe a [Failure[?]]
    }

    they should "not treat partial ranges as errors" in {
        cls.parse("[-]") shouldBe Success(Class(Diet.one('-')))
        cls.parse("[a-]") shouldBe Success(Class(Diet.one('a').add('-')))
        cls.parse("[-a]") shouldBe Success(Class(Diet.one('-').add('a')))
    }

    they should "accept ranges otherwise" in {
        cls.parse("[a-c]") shouldBe Success(Class(Diet.fromRange(Range('a', 'c'))))
        cls.parse("[!--]") shouldBe Success(Class(Diet.fromRange(Range('!', '-'))))
    }

    they should "reject ill-formed ranges" in {
        cls.parse("[c-a]") shouldBe a [Failure[?]]
    }

    they should "allow for set unions" in {
        cls.parse("[[a-c][d-f]]") shouldBe Success(Class(Diet.fromRange(Range('a', 'f'))))
        cls.parse("[a-z[0-9]]") shouldBe Success(Class(Diet.fromRange(Range('a', 'z')).addRange(Range('0', '9'))))
        cls.parse("[a-z0-9]") shouldBe Success(Class(Diet.fromRange(Range('a', 'z')).addRange(Range('0', '9'))))
    }

    they should "allow for set intersections" in {
        cls.parse("[&&a]") shouldBe Success(Class(Diet.one('a')))
        cls.parse("[a&&]") shouldBe Success(Class(Diet.one('a')))
        cls.parse("[a-z&&0-9]") shouldBe Success(Class(Diet.empty))
        cls.parse("[a-z&&a-z]") shouldBe Success(Class(Diet.fromRange(Range('a', 'z'))))
        cls.parse("[a-z&&&&&&0-9]") shouldBe Success(Class(Diet.empty))
        cls.parse("[a-z&&&&&&a-z]") shouldBe Success(Class(Diet.fromRange(Range('a', 'z'))))
    }

    they should "reject empty intersections" in {
        cls.parse("[&&]") shouldBe a [Failure[?]]
    }

    // TODO: more tests when escape sequences are implemented
}
