package oregano.bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import oregano.regex

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)

class MatchWithCapsBenchmark {

  val inputGreedyCapture = "a" * 1000 + "b"
  val inputAlternationHit = "grault"
  val inputAlternationFail = "foo" * 1000 + "nope"
  val inputAmbiguous = "a" * 20 + "b"
  val inputRepeatingLiteral = "ab" * 1000 + "c"
  val inputManyGroups = "abcdefghij"

  val patternGreedyCapture = "(a*)b".regex
  val patternAlternationHit = "(foo|bar|baz|qux|quux|grault|garply|waldo|fred)".regex
  val patternAlternationFail = "(foo|bar|baz)*quux".regex
  val patternAmbiguous = "(a|aa)*b".regex
  val patternRepeatingLiteral = "(ab)*c".regex
  val patternManyGroups = "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)".regex

  val expectedGreedyCapture = Some(Array(0, 1001, 0, 1000))
  val expectedAlternationHit = Some(Array(0, 6, 0, 6))
  val expectedAlternationFail = None
  val expectedAmbiguous = Some(Array(0, 21, 19, 20))
  val expectedRepeatingLiteral = Some(Array(0, 201, 198, 200))
  val expectedManyGroups = Some(Array(0, 10, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10))

  @Setup(Level.Trial)
  def validateCorrectness(): Unit = {
    def check(output: Option[Array[Int]], expected: Option[Array[Int]]): Unit = {
      def normalize(opt: Option[Array[Int]]): Option[List[Int]] =
        opt.map(_.toList)

      if (normalize(output) != normalize(expected)) {
        throw new AssertionError(s"Expected $expected, but got $output")
      }
    }

    check(patternGreedyCapture.matchesWithCaps(inputGreedyCapture), expectedGreedyCapture)
    check(patternAlternationHit.matchesWithCaps(inputAlternationHit), expectedAlternationHit)
    check(patternAlternationFail.matchesWithCaps(inputAlternationFail), expectedAlternationFail)
    check(patternAmbiguous.matchesWithCaps(inputAmbiguous), expectedAmbiguous)
    check(patternRepeatingLiteral.matchesWithCaps(inputRepeatingLiteral), expectedRepeatingLiteral)
    check(patternManyGroups.matchesWithCaps(inputManyGroups), expectedManyGroups)
  }


  // @Benchmark def matchGreedyCapture(): Option[Array[Int]] =
  //   patternGreedyCapture.matchesWithCaps(inputGreedyCapture)

  // @Benchmark def matchAlternationHit(): Option[Array[Int]] =
  //   patternAlternationHit.matchesWithCaps(inputAlternationHit)

  // @Benchmark def matchAlternationFail(): Option[Array[Int]] =
  //   patternAlternationFail.matchesWithCaps(inputAlternationFail)

  // @Benchmark def matchAmbiguousBacktrack(): Option[Array[Int]] =
  //   patternAmbiguous.matchesWithCaps(inputAmbiguous)

  @Benchmark def matchRepeatingLiteral(): Option[Array[Int]] =
    patternRepeatingLiteral.matchesWithCaps(inputRepeatingLiteral)

  // @Benchmark def matchManyGroup(): Option[Array[Int]] =
  //   patternManyGroups.matchesWithCaps(inputManyGroups)
}
