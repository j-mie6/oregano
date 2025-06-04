
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.*
import oregano.internal.StagedMatchers.{stagedProg, stagedProgWithCaps}


class StagedProgMatcherTests extends AnyFlatSpec {

  val nestedMatcher = stagedProg("(ab)*bc|(def)")

  behavior of "StagedMatcher - regex (ab)*bc|(def)"

  it should "match valid strings for the first alternative (ab)*bc including backtracking" in {
    val validFirstAlt = Table(
      "input",
      "ababbc",
      "abababababbc",
      "bc",
      "abababbc",
      "abbc",
    )

    forAll(validFirstAlt) { str =>
      withClue(s"Failed on input: $str") {
        nestedMatcher(str) shouldBe true
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
        nestedMatcher(str) shouldBe true
      }
    }
  }

  it should "reject invalid or partial matches" in {
    val invalidInputs = Table(
      "input",
      "",
      "defg",
      "de"
    )

    forAll(invalidInputs) { str =>
      withClue(s"Incorrectly matched input: $str") {
        nestedMatcher(str) shouldBe false
      }
    }
  }

  val groupingMatcher = stagedProg("(a|b)*c[0-9]")

  behavior of "StagedProgMatcher - regex (a|b)*c[0-9]"

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
        groupingMatcher(str) shouldBe true
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
        groupingMatcher(str) shouldBe false
      }
    }
  }


  val matcherWithBigClass = stagedProg("[a-zA-Z0-9_.+-]*")

  behavior of "StagedCPSMatcher - regex [a-zA-Z0-9_.+-]*"

  it should "match valid strings" in {
    val validInputs = Table(
      "input",
      "user.name+tag-123",
      "abc123XYZ",
      "a_b-c.d+e"
    )

    forAll(validInputs) { str =>
      withClue(s"Should have matched: $str") {
        matcherWithBigClass(str) shouldBe true
      }
    }
  }

  it should "reject invalid strings" in {
    val invalidInputs = Table(
      "input",
      "abc123!",  
      "user@name", 
      "tag#123",   
      "invalid space"
    )

    forAll(invalidInputs) { str =>
      withClue(s"Should NOT have matched: $str") {
        matcherWithBigClass(str) shouldBe false
      }
    }
  }

  val complexExpressionMatcher = stagedProg("((ab)*|[cd]*)e(f|g)[0-9]")

  behavior of "StagedProgMatcher - ((ab)*|[cd]*)e(f|g)[0-9]"

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
        complexExpressionMatcher(str) shouldBe true
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
        complexExpressionMatcher(str) shouldBe false
      }
    }
  }

  val heavyBacktrackingMatcher = stagedProg("((a|aa)*)b")

  behavior of "StagedProgMatcher - ((a|aa)*)b"

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
        heavyBacktrackingMatcher(str) shouldBe true
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
        heavyBacktrackingMatcher(str) shouldBe false
      }
    }
  }

  val matcherCapsWithSequentialLoops = stagedProgWithCaps("((a*)b*)bc|(def)")

  behavior of "StagedProgMatcher - matchesWithCaps - ((a*)b*)bc|(def)"

  it should "match expected capture groups for each input" in {
    val cases = Table(
      ("input", "expectedCaps"),
      ("a", None),
      ("aabbc", Some(Array(0, 5, 0, 3, 0, 2, -1, -1))),
      ("abc", Some(Array(0, 3, 0, 1, 0, 1, -1, -1))),
      ("abbc", Some(Array(0, 4, 0, 2, 0, 1, -1, -1))),
      ("abbbc", Some(Array(0, 5, 0, 3, 0, 1, -1, -1))),
      ("aaaaabaababbc", None),
      ("aaaaabc", Some(Array(0, 7, 0, 5, 0, 5, -1, -1))),
      ("bc", Some(Array(0, 2, 0, 0, 0, 0, -1, -1))), 
      ("abc", Some(Array(0, 3, 0, 1, 0, 1, -1, -1))),
      ("def", Some(Array(0, 3, -1, -1, -1, -1, 0, 3))),
      ("", None),
      ("defg", None),
      ("de", None)
    )

    forAll(cases) { (input, expectedOpt) =>
      val actualOpt = matcherCapsWithSequentialLoops(input)

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

  val matcherCapsWithNestedAltLoops = stagedProgWithCaps("(((a)|b|cd)*)e")

  behavior of "StagedProgMatcher - matchesWithCaps - (((a)|b|cd)*)e"

  it should "match expected capture groups for each input" in {
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

    forAll(cases) { (input, expectedOpt) =>
      val actualOpt = matcherCapsWithNestedAltLoops(input)

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

