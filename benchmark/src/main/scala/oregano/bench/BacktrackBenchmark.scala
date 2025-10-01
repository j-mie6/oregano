
package oregano.bench

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

import oregano.regex 

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class BacktrackBenchmark {

  // Input Strings
  val inputBacktrackTrap = "a" * 20 + "b"
  val inputFailLargeClass = "abc123!"
  val inputBigClass = "user.name+tag-123"
  val inputAlternationHit = "grault"
  val inputAlternationFailLate = ("foo" * 1000) + "nope"

  // Precompiled Regex Patterns
  val regexBacktrackTrap = "(a|aa)*b".regex
  val regexFailLargeClass = "[a-zA-Z0-9]*".regex
  val regexBigClass = "[a-zA-Z0-9_.+-]*".regex
  val regexAlternationHit = "(foo|bar|baz|qux|quux|corge|grault|garply|waldo|fred)".regex
  val regexAlternationFailLate = "(foo|bar|baz)*quux".regex

  @Setup(Level.Trial)
  def validateCorrectness(): Unit = {
    inline def check(inline pattern: String, input: String): Unit = {
      val expected = pattern.r.matches(input)
      val actual = pattern.regex.matches(input)
      assert(actual == expected, s"Mismatch on '$pattern' vs '$input' — expected: $expected, got: $actual")
    }

    check("(a|aa)*b", inputBacktrackTrap)
    check("[a-zA-Z0-9]*", inputFailLargeClass)
    check("[a-zA-Z0-9_.+-]*", inputBigClass)
    check("(foo|bar|baz|qux|quux|corge|grault|garply|waldo|fred)", inputAlternationHit)
    check("(foo|bar|baz)*quux", inputAlternationFailLate)
  }   

  @Benchmark
  def matchBacktrackTrap(): Boolean =
    regexBacktrackTrap.matches(inputBacktrackTrap)

  @Benchmark
  def matchFailLargeClass(): Boolean =
    regexFailLargeClass.matches(inputFailLargeClass)

  @Benchmark
  def matchBigClass(): Boolean =
    regexBigClass.matches(inputBigClass)

  @Benchmark
  def matchAlternationHit(): Boolean =
    regexAlternationHit.matches(inputAlternationHit)

  @Benchmark
  def matchAlternationFailLate(): Boolean =
    regexAlternationFailLate.matches(inputAlternationFailLate)
}
