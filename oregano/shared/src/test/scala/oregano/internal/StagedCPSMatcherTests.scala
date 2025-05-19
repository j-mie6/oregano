import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.*
import scala.quoted.staging.*
import oregano.internal.{Pattern, PatternResult, CPSMatcher}
import org.scalatest.SequentialNestedSuiteExecution

given Compiler = Compiler.make(classOf[StagedCPSMatcherTests].getClassLoader)

inline def stagedMatcherFrom(regex: String): CharSequence => Boolean =
  run {
    val PatternResult(pattern, _, _, _) = Pattern.compile(regex)
    CPSMatcher.genMatcherPattern(pattern)
  }

class StagedCPSMatcherTests extends AnyFlatSpec with SequentialNestedSuiteExecution {

  val nestedMatcher = stagedMatcherFrom("((a*)b*)*bc|(def)")

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

  val groupingMatcher = stagedMatcherFrom("(a|b)*c[0-9]")

  behavior of "CPSMatcher - regex (a|b)*c[0-9]"

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


  val complexExpressionMatcher = stagedMatcherFrom("((ab)*|[cd]*)e(f|g)[0-9]")

  behavior of "CPSMatcher - ((ab)*|[cd]*)e(f|g)[0-9]"

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

  val heavyBacktrackingMatcher = stagedMatcherFrom("((a|aa)*)b")

  behavior of "CPSMatcher - ((a|aa)*)b"

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
}

