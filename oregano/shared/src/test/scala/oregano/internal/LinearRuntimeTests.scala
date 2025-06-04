package oregano.internal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.*

inline def getMachine(inline regEx: String): RE2Machine = {
  val PatternResult(pattern, groupCount, _, _) =
    Pattern.compile(regEx)
  val prog = ProgramCompiler.compileRegexp(pattern, groupCount)
  RE2Machine(prog)
}

class RuntimeLinearMatcherTests_NestedLoops extends AnyFlatSpec {

  val machine = getMachine("((a*)b*)*bc|(def)")

  behavior of "RE2Machine - regex ((a*)b*)*bc|(def)"

  it should "match valid strings for the first alternative ((a*)b*)*bc including backtracking" in {
    val validFirstAlt = Table(
      "input",
      "ababc",
      "aaaaabaababbc",
      "bc",
      "abbbbbc",
      "abababbbbbc",
      "abc",
      "abbbc"
    )

    forAll(validFirstAlt) { str =>
      withClue(s"Failed on input: $str") {
        machine.matches(str) shouldBe true
      }
    }
  }

  it should "match valid strings for the second alternative (def)" in {
    val validSecondAlt = Table(
      "input",
      "def"
    )

    forAll(validSecondAlt) { str =>
      withClue(s"Failed on input: $str") {
        machine.matches(str) shouldBe true
      }
    }
  }

  it should "reject invalid or partial matches" in {
    val invalidInputs = Table(
      "input",
      "defgih",
      "",
      "de"
    )

    forAll(invalidInputs) { str =>
      withClue(s"Incorrectly matched input: $str") {
        machine.matches(str) shouldBe false
      }
    }
  }
}

class RuntimeLinearMatcherTests_Grouping extends AnyFlatSpec {

  val machine = getMachine("(a|b)*c[0-9]")

  behavior of "LinearMatcher - regex (a|b)*c[0-9]"

  it should "match valid strings" in {
    val validInputs = Table(
      "input",
      "c0",
      "ac5",
      "bbbac9",
      "ababc2"
    )

    forAll(validInputs) { str =>
      withClue(s"Should have matched: $str") {
        machine.matches(str) shouldBe true
      }
    }
  }

  it should "reject invalid strings" in {
    val invalidInputs = Table(
      "input",
      "abc",  
      "c",    
      "d0",   
      "aac",  
      "a0"    
    )

    forAll(invalidInputs) { str =>
      withClue(s"Should not have matched: $str") {
        machine.matches(str) shouldBe false
      }
    }
  }
}

class RuntimeLinearMatcherTests_ComplexExpression extends AnyFlatSpec {

  val machine = getMachine("((ab)*|[cd]*)e(f|g)[0-9]")

  behavior of "LinearMatcher - ((ab)*|[cd]*)e(f|g)[0-9]"

  it should "match valid strings" in {
    val validInputs = Table(
      "input",
      "cdef3",
      "ababef9",
      "abeg2",
      "cdeg5",
      "abef7",
      "ef0", 
      "cdcdeg3"
    )

    forAll(validInputs) { str =>
      withClue(s"Should have matched: $str") {
        machine.matches(str) shouldBe true
      }
    }
  }

  it should "reject invalid strings" in {
    val invalidInputs = Table(
      "input",
      "abe",       
      "abef",      
      "abgh5",     
      "xyzef0",
      "cdfg3",
      "abeg",      
      "eg",        
      "ab9"        
    )

    forAll(invalidInputs) { str =>
      withClue(s"Should NOT have matched: $str") {
        machine.matches(str) shouldBe false
      }
    }
  }
}

class RuntimeLinearMatcherTests_BacktrackingHeavy extends AnyFlatSpec {

  val machine = getMachine("((a|aa)*)b")

  behavior of "LinearMatcher - ((a|aa)*)b"

  it should "match valid strings with heavy backtracking" in {
    val validInputs = Table(
      "input",
      "b",
      "ab",
      "aab",
      "aaab",
      "aaaab",
      "aaaaab",
      "aaaaaab",
      "aaaaaaaaaab"
    )

    forAll(validInputs) { str =>
      withClue(s"Should have matched: $str") {
        machine.matches(str) shouldBe true
      }
    }
  }

  it should "reject invalid strings with ambiguous prefixes" in {
    val invalidInputs = Table(
      "input",
      "a",
      "aa",
      "aaa",
      "aaaaa",
      "ba",
      "c",
      "aac"
    )

    forAll(invalidInputs) { str =>
      withClue(s"Should NOT have matched: $str") {
        machine.matches(str) shouldBe false
      }
    }
  }
}

class RuntimeLinearMatcherTests_WithCapturesNestedLoops extends AnyFlatSpec {

  val machine = getMachine("((a*)b*)*bc|(def)")

  behavior of "LinearMatcher.matchesWithCaps - ((a*)b*)*bc|(def)"

  val cases = Table(
    ("input", "expectedCaps"),
    ("a", None),
    ("ababc", Some(Array(0, 5, 2, 3, 2, 3, -1, -1))),
    ("abc", Some(Array(0, 3, 0, 1, 0, 1, -1, -1))),
    ("abbc", Some(Array(0, 4, 0, 2, 0, 1, -1, -1))),
    ("abbbc", Some(Array(0, 5, 0, 3, 0, 1, -1, -1))),
    ("aaaaabaababbc", Some(Array(0, 13, 9, 11, 9, 10, -1, -1))),
    ("ababc", Some(Array(0, 5, 2, 3, 2, 3, -1, -1))),
    ("bc", Some(Array(0, 2, -1, -1, -1, -1, -1, -1))),
    ("abc", Some(Array(0, 3, 0, 1, 0, 1, -1, -1))),
    ("def", Some(Array(0, 3, -1, -1, -1, -1, 0, 3))),
    ("ababbbbabbbbabbabc", Some(Array(0, 18, 15, 16, 15, 16, -1, -1))),
    ("", None),
    ("defg", None),
    ("de", None)
  )

  it should "match expected capture groups for each input" in {
    forAll(cases) { (input, expectedOpt) =>
      val matchRes = machine.matches(input)
      val actualOpt = if matchRes then Some(machine.matchcap) else None

      withClue(s"Input: '$input'") {
        (actualOpt, expectedOpt) match {
          case (Some(actual), Some(expected)) =>
            actual.toList shouldBe expected.toList

          case (None, None) => succeed

          case _ =>
            fail(s"Expected: $expectedOpt, got: $actualOpt")
        }
      }
    }
  }
}

class RuntimeLinearMatcherTests_NestedAltRep extends AnyFlatSpec {

  val machine = getMachine("(((a)|b|cd)*)e")

  behavior of "LinearMatcher.matchesWithCaps - (((a)|b|cd)*)e"

  val cases = Table(
    ("input", "expectedCaps"),
    ("e", Some(Array(0, 1, 0, 0, -1, -1, -1, -1))),                   
    ("ae", Some(Array(0, 2, 0, 1, 0, 1, 0, 1))),                    
    ("abe", Some(Array(0, 3, 0, 2, 1, 2, 0, 1))),                   
    ("cde", Some(Array(0, 3, 0, 2, 0, 2, -1, -1))),                   
    ("ababe", Some(Array(0, 5, 0, 4, 3, 4, 2, 3))),                 
    ("abcdcde", Some(Array(0, 7, 0, 6, 4, 6, 0, 1))),               
    ("ababcdcde", Some(Array(0, 9, 0, 8, 6, 8, 2, 3))),             
    ("", None),                                               
    ("ab", None),                                             
    ("abc", None)                                             
  )

  it should "match expected capture groups for each input" in {
    forAll(cases) { (input, expectedOpt) =>
      val matchRes = machine.matches(input)
      val actualOpt = if matchRes then Some(machine.matchcap) else None

      withClue(s"Input: '$input'") {
        (actualOpt, expectedOpt) match {
          case (Some(actual), Some(expected)) =>
            actual.toList shouldBe expected.toList

          case (None, None) => succeed

          case _ =>
            fail(s"Expected: $expectedOpt, got: $actualOpt")
        }
      }
    }
  }
}
