package oregano.bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import oregano.regex 

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class BranchingBenchmark:
  inline val pattern = "abc|def|ghi|jkl|mnop|qrst|uvwx|yzab|cdef|ghij"
  val stagedRegex = pattern.regex
  // val patternRuntime = "abc|def|ghi|jkl|mnop|qrst|uvwx|yzab|cdef|ghij" 
  // val unstagedRegex = patternRuntime.regex
  val testInputs = Array("abc", "mnop", "qrst", "uvwx", "ghij", "nope", "wrong", "xyz")
  var inputIndex = 0

  private def nextInput(): String = {
    val str = testInputs(inputIndex)
    inputIndex = (inputIndex + 1) % testInputs.length
    str
  }

  // @Benchmark
  // def javaImpl(): Boolean = {
  //   unstagedRegex.matches(nextInput())
  // }

  @Benchmark
  def linear(): Boolean = {
    stagedRegex.matchesLinear(nextInput())
  }

  // @Benchmark
  // def backtrack(): Boolean = {
  //   stagedRegex.matchesBacktrack(nextInput())
  // }

  // @Benchmark
  // def backtrackCPS(): Boolean = {
  //   stagedRegex.matches(nextInput())
  // }

// //   @Benchmark
// //   def runtimeLinear(): Boolean = {
// //     stagedRegex.matchesRuntimeLinear(nextInput())
// //   }

// //   @Benchmark
// //   def runtimeBacktrack(): Boolean = {
// //     stagedRegex.matchesRuntimeBacktrack(nextInput())
// //   }
// // }

