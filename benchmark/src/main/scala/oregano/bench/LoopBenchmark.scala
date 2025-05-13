package oregano.bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized
import oregano.regex

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class LoopBenchmark {
  var stagedRegex = "a".regex
  val notInlined = "a"
  var runtimeRegex = notInlined.r
  var testInput: String = uninitialized

  @Setup(Level.Iteration)
  def setup(): Unit = {
    // inline val input = "(a|b)*"
    inline val input = "(a|b)*ababababababababa"
    val stagedRegex = input.regex
    val inputCopy = "(a|b)*" 
    val runtimeRegex = inputCopy.r
    testInput = "ababababababa" * 1000
  }

  @Benchmark
  def javaImpl(): Unit = {
    val result = runtimeRegex.matches(testInput)
  }

  // @Benchmark
  // def backtrackCPS(): Unit = {
  //   val result = stagedRegex.matches(testInput)
  // }

  // @Benchmark
  // def linear(): Unit = {
  //   val _ = stagedRegex.matchesLinear(testInput)
  // }

  @Benchmark
  def backtrackProg(): Unit = {
    val result = stagedRegex.matchesBacktrack(testInput)
  }


  // @Benchmark
  // def backtrackProgLambda(): Unit = {
  //   val _ = stagedRegex.matches(testInput)
  // }

  // @Benchmark
  // def runtimeRegexBacktrackCPS(): Boolean = {
  //   runtimeRegex.matches(testInput)
  // }

  // @Benchmark
  // def runtimeRegexBacktrackProg(): Boolean = {
  //   runtimeRegex.matchesBacktrack(testInput)
  // }
}
