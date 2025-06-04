package oregano.bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized
import oregano.regex

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class LoopBenchmark {
  inline val input = "(a|b)*"
  val stagedRegex = input.regex
  val inputCopy = "(a|b)*" 
  val runtimeRegex = inputCopy.regex
  val testInput = "ababababababa" * 10

  // @Benchmark
  // def javaImpl(): Unit = {
  //   val result = runtimeRegex.matches(testInput)
  // }

  // @Benchmark
  // def backtrackCPS(): Unit = {
  //   val result = stagedRegex.matches(testInput)
  // }

  // @Benchmark
  // def StagedLinearNoCap(): Unit = {
  //   val result = stagedRegex.matches(testInput)
  // }

  @Benchmark
  def unstagedLinearRE2(): Unit = {
    val _ = stagedRegex.matchesLinear(testInput)
  }

  // @Benchmark
  // def backtrackProg(): Unit = {
  //   val result = stagedRegex.matchesBacktrack(testInput)
  // }$stepLoopExpr(compileTimeMachine, input)


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
