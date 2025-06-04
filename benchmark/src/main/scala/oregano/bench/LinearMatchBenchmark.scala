package oregano.bench

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

import oregano.regex

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class LinearMatchBenchmark {

  val alphaOnly = "a" * 25
  val alphaNum = ("abc123XYZ" * 30) 
  val classSymbols = ("abc123.-_" * 25) + "Z"
  val failFinalChar = "x" * 29 + "!"
  val tokenStream = ("token:" * 35) + "token" 

  // Precompiled regexes (no capture groups)
  val regexAlphaStar = "[a]*".regex
  val regexAlphaNumStar = "[a-zA-Z0-9]*".regex
  val regexClassSymbols = "[a-zA-Z0-9._-]*".regex
  val regexRejectFinalChar = "[x]*".regex
  val regexTokenLoop = "(token:)*token".regex

  @Setup(Level.Trial)
  def validateCorrectness(): Unit = {
    inline def check(inline pattern: String, input: String): Unit = {
      val expected = pattern.r.matches(input)
      val actual = pattern.regex.matchesLinear(input)
      assert(actual == expected, s"Mismatch on '$pattern' vs '$input' — expected: $expected, got: $actual")
    }

    check("[a]*", alphaOnly)
    check("[a-zA-Z0-9]*", alphaNum)
    check("[a-zA-Z0-9._-]*", classSymbols)
    check("[x]*", failFinalChar)
    check("(token:)*token", tokenStream)
  }

  @Benchmark def matchAlphaStar(): Boolean = 
    regexAlphaStar.matchesLinear(alphaOnly)

  @Benchmark def matchAlphaNumStar(): Boolean = 
    regexAlphaNumStar.matchesLinear(alphaNum)

  @Benchmark def matchClassSymbols(): Boolean = 
    regexClassSymbols.matchesLinear(classSymbols)

  @Benchmark def matchFailFinalChar(): Boolean = 
    regexRejectFinalChar.matchesLinear(failFinalChar)

  @Benchmark def matchTokenLoop(): Boolean = 
    regexTokenLoop.matchesLinear(tokenStream)
}
