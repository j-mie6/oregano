// package oregano.bench

// import org.openjdk.jmh.annotations.*
// import java.util.concurrent.TimeUnit
// import scala.compiletime.uninitialized
// import oregano.regex 

// @BenchmarkMode(Array(Mode.Throughput))
// @OutputTimeUnit(TimeUnit.MILLISECONDS)
// @State(Scope.Thread)
// class BranchingBenchmark:
//   var stagedRegex = "a".regex
//   var testInputs: Array[String] = uninitialized
//   var inputIndex: Int = uninitialized

//   @Setup(Level.Iteration)
//   def setup(): Unit = {
//     inline val pattern = "abc|def|ghi|jkl|mnop|qrst|uvwx|yzab|cdef|ghij"
//     stagedRegex = pattern.regex
//     testInputs = Array("abc", "mnop", "qrst", "uvwx", "ghij", "nope", "wrong", "xyz")
//     inputIndex = 0
//   }

//   private def nextInput(): String = {
//     val str = testInputs(inputIndex)
//     inputIndex = (inputIndex + 1) % testInputs.length
//     str
//   }

//   @Benchmark
//   def javaImpl(): Boolean = {
//     stagedRegex.matches(nextInput())
//   }
// }

  // @Benchmark
  // def linear(): Boolean = {
  //   stagedRegex.matchesLinear(nextInput())
  // }

// //   @Benchmark
// //   def backtrack(): Boolean = {
// //     stagedRegex.matchesBacktrack(nextInput())
// //   }

// //   @Benchmark
// //   def runtimeLinear(): Boolean = {
// //     stagedRegex.matchesRuntimeLinear(nextInput())
// //   }

// //   @Benchmark
// //   def runtimeBacktrack(): Boolean = {
// //     stagedRegex.matchesRuntimeBacktrack(nextInput())
// //   }
// // }

