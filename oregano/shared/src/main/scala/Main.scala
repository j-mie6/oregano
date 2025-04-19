package oregano.shared

import oregano.regex 
import oregano.internal.makeMatch

object Main {
  def main(args: Array[String]): Unit = {
    // inline val regEx = "\\u0061\\0142c|def|ghi"
    inline val regEx = "ababc"
    println("Current inlined regex: " + regEx)
    val compileTime = regEx.regex
    println(s"matches \"ababc\": ${compileTime.matches("ababc")}")
    println(s"matches \"def\": ${compileTime.matches("def")}")
    println(s"matches \"ghi\": ${compileTime.matches("ghi")}")
    println(s"matches \"jkl\": ${compileTime.matches("jkl")}")
    val uninlined = "abc|def"
    println("Current uninlined regex: " + uninlined)
    val runtime = uninlined.regex
    println(s"matches \"abc\": ${runtime.matches("abc")}")
    println(s"matches \"def\": ${runtime.matches("def")}")
    println(s"matches \"ghi\": ${runtime.matches("ghi")}")
  }

//   def main(args: Array[String]): Unit = {
//     // Inline/staged regex
//     inline val regEx = "abc|def|ghi|jkl|mnop|qrst|uvwx|yzab|cdef|ghij"
//     val compileTime = regEx.regex

//     // Regular Scala runtime regex
//     val uninlined = "abc|def|ghi|jkl|mnop|qrst|uvwx|yzab|cdef|ghij"
//     val runtime = uninlined.r

//     val iterations = 10000000
//     val testInputs = Array("abc", "mnop", "qrst", "uvwx", "ghij", "nope", "wrong", "xyz")

//     def benchmark(name: String, matcher: String => Boolean): Unit = {
//       val start = System.nanoTime()
//       var i = 0
//       while (i < iterations) {
//         testInputs.foreach(matcher)
//         i += 1
//       }
//       val end = System.nanoTime()
//       println(f"$name benchmark: ${(end - start) / 1e6}%.2f ms")
//     }

//     println("== Match correctness check ==")
//     testInputs.foreach { input =>
//       println(s"Staged:   matches '$input' => ${compileTime.matches(input)}")
//       println(s"Runtime:  matches '$input' => ${runtime.matches(input)}")
//     }

//     println("\n== Benchmarking ==")
//     benchmark("Staged", compileTime.matches)
//     benchmark("Runtime", runtime.matches)
//   }
}